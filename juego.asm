;*******************************************************************************************
;*                                    Juego Tetris                                                                                                                                                 *
;*******************************************************************************************
	org 7c00h ;Con ORG se le indica al programa que debe iniciar en esta posición

;*******************************************************************************************
;*                                    Creación de macros                                                                                                                                *
;*******************************************************************************************

;--------------------------------  MACRO #1  -----------------------------------------------
;Macro-1: imp_reg .
;	Recibe 1 parametro de entrada:
;		%1 es la direccion del registro a imprimir
;		
;--------------------------------------------------------------------------------------------

%macro imp_reg 1 
	mov dx, %1 ; mueva el parámetro de entrada al registro de datos "dx"
	mov cx, 16 ; mueva el valor al registro contador "cx"
imp_reg_loop:
	push cx ; La instrucción PUSH decrementa en dos el valor deL registro apuntador de pila
	;y posteriormente transfiere el contenido del operando fuente a la nueva dirección resultante en el registro recién modificado.
	mov al, '0' ; mueve '0' a la parte baja del registro acomulador
	test dh, 10000000b ;
	
	;_____________________________________________________________________
        ;TEST: Realiza una conjunción, bit por bit, de los operandos, 
        ;a diferencia del "and"  no coloca el resultado en el operando destino, 
        ; sino que solo tiene efecto sobre el estado de las banderas
        ;____________________________________________________________________
       
	jz imp_reg_do ; Si la vandera es cero salta a la etiqueta imp_reg_do
	mov al, '1' ;  mueve '1' a la parte baja del registro acomulador
imp_reg_do:
	mov bx, 0x0006             ; página = 0 (BH), color = gris en negro (BL)
	mov ah, 0x09               ; Escribir el caracter almacenado en AL
	mov cx, 1                  ; Mueve el valor de 1 al registro contador
	int 0x10                   ; Esta interrupción se utiliza para mostrar texto en la pantalla (sin la necesidad de llamar a la int 80), para cambiar a modo gráfico, para establecer la paleta de colores, etc...
	mov ah, 3                  ; mover el cursor una columna hacia adelante
	int 0x10		   ; Se vuelve a mostrar el texto en pantalla
	inc dx                     ; La instrucción suma 1 al operando destino y guarda el resultado en el mismo operando destino.
	mov ah, 2                  ; Establecer cursor
	int 0x10		   ; Esta interrupción se utiliza para mostrar texto en la pantalla
	pop cx                     ; Toma de la pila el último valor
	shl dx, 1                  ; Desplazamiento de 1 bit a la izquierda
	loop imp_reg_loop        
	jmp $
%endmacro
;----------------------------------------------------------------------------------------------
;--------------------------------  MACRO #2  --------------------------------------------------
;Macro-2: sleep.
;	Recibe 1 parametro de entrada:	
;       Pausar por una determinada cantidad de microsegundos
;---------------------------------------------------------------------------------------------------------
%macro sleep 1
	pusha ; Almacena en la pila los registros básicos y se sigue este orden : AX,CX,DX,BX,SP,BP,SI,DI
	xor cx, cx ; limpieza del registro contador "cx"
	mov dx, %1 ; mueve al registro de datos la entrada del macro
	mov ah, 0x86 ; 	Esperar intervalo de microsegundos (BIOS)
	int 0x15	 ; interrupción de tiempo
	popa      ; extrae de la pila los registros básicos (orden inverso al pusha
%endmacro
;---------------------------------------------------------------------------------------------------------
;--------------------------------  MACRO #3  -------------------------------------------------------------
;Macro-3: selecionar_bloque	
;       Selección de un bloque de manera aleatoria
;---------------------------------------------------------------------------------------------------------
%macro selecionar_bloque 0
	mov ah, 2  ;  con la "int 0x1a" se obtiene la hora actual del sistema
	int 0x1a
	mov al, byte [valor_semilla]
	xor ax, dx
	mov bl, 31
	mul bx
	inc ax ; La instrucción suma 1 al operando destino y guarda el resultado en el mismo operando destino.
	mov byte [valor_semilla], al
	xor dx, dx
	mov bx, 7
	div bx
	shl dl, 3 ; 
	xchg ax, dx; Intercambia el contenido de dos registros, o bien el contenido de un registro y el de una posición de memoria.
	;Sintaxis: XCHG registro, registro/memoria                 
        ; mov al, dl
%endmacro
;---------------------------------------------------------------------------------------------------------
;--------------------------------  MACRO #4  -------------------------------------------------------------
;Macro-4: limp_pantalla
;        Establece el modo de video y oculta el cursor
;---------------------------------------------------------------------------------------------------------
%macro limp_pantalla 0
	xor ax, ax   ; Limpia la pantalla de (40x25)
	int 0x10     ; Esta interrupción se utiliza para mostrar texto en la pantalla
	mov ah, 1    ; Ocultar el cursor
	mov cx, 0x2607
	int 0x10     ; Esta interrupción se utiliza para mostrar texto en la pantalla (control de pantalla)
%endmacro
;---------------------------------------------------------------------------------------------------------
;*********************************************************************************************************
;*                                      Variables sin inicializar                                        *
;*********************************************************************************************************
campo_col_izq:  equ 13  ; Columna izquierda
ancho_campo:     equ 14  
ancho_interior:     equ 12
interior_primer_col: equ 14  ; primera columna
inicio_fila_col:   equ 0x0412 ; fila y columna
;*********************************************************************************************************
;--------------------------------  MACRO #5  -------------------------------------------------------------
;Macro-5: iniciar_pantalla
;       Inicializa la pantalla
;---------------------------------------------------------------------------------------------------------
%macro iniciar_pantalla 0
	limp_pantalla			 ; Se llama al macro para limpiar la pantalla
	mov dh, 3                        ; fila
	mov cx, 18                       ; número de filas 
ia: push cx				 ; 
	inc dh                           ; incrementa la fila
	mov dl, campo_col_izq           ; Establecer columna
	mov cx, ancho_campo              ; Ancho de caja
	mov bx, 0x33                     ; color turquesa para los bordes del juego
	call set_and_write
	cmp dh, 21                       ; No eliminar la última línea
	je ib                            ; Si llega a la última línea, salta
	inc dx                           ; Incrementar la columna
	mov cx, ancho_interior              ; Ancho de caja
	xor bx, bx                       ; color
	call set_and_write               
ib: pop cx                               ; transfiere el último valor almacenado en la pila
	loop ia
%endmacro
;---------------------------------------------------------------------------------------------------------
;*********************************************************************************************************
;*                                            Variables sin inicializar                                                                                                                                                *
;*********************************************************************************************************
delay:      equ 0x7f00
valor_semilla: equ 0x7f02
;*********************************************************************************************************

;*********************************************************************************************************
;*                                            Segmento de código                                         *
;*********************************************************************************************************
section .text

;_________________________________________________________________________________________________________
iniciar_juego:   ; Etiqueta para iniciar el juego
	xor ax, ax ; Limpieza del registro acomulador
	mov ds, ax ; mueve lo que hay en el registro acomulador al registro de segmento de datos
	iniciar_pantalla ; Se llama al macro que inicializa la pantalla
;_________________________________________________________________________________________________________
nuevo_bloque: ; Etiqueta para crear un nuevo ladrillo
	mov byte [delay], 100            ;  calculo para un delay de (3 * 100) 300ms
	selecionar_bloque                     ; Devuelve el ladrillo seleccionado en AL
	mov dx, inicio_fila_col            ; inicia en la fila 4,  columna 38
;_________________________________________________________________________________________________________
lp: ; Etiqueta para chequear la colisión y establecer se pierde la partida
	call revisar_colision
	jne $                            ; En caso de colisión se finaliza el juego
	call imp_bloque	         ; Si no entonces se imprime otro bloque
;________________________________________________________________________________________________________
esperar_tecla: ; 
	xor cx, cx 
	mov cl, byte [delay] 
;________________________________________________________________________________________________________
esperar:         
	push cx 
	sleep 3000                       ; Se llama al macro para esperar 3ms

	push ax			         
	mov ah, 1                        ; comprobar si hay pulsación de tecla; AX modificado
					
	int 0x16                         ;  Esta interrupción se encarga de controlar el teclado del PC.
	mov cx, ax
	pop ax
	jz no_tecla                    ; Si no se presiona una tecla
	call borrar_bloque             ; llamada a al macro para borrar el ladrillo
                                     ; 4b left, 48 up, 4d right, 50 down
	cmp ch, 0x4b                 ; flecha izquierda
	je flecha_izq                ; 
	cmp ch, 0x48                 ; flecha derecha
	je flecha_arriba
	cmp ch, 0x4d
	je flecha_der               ; Salto a la etiqueta flecha_der 

	mov byte [delay], 10         ; cualquier otra tecla es para bajar rápido el ladrillo
	jmp limpiar_tecla               ; salto a limpieza de tecla
flecha_izq:
	dec dx
	call revisar_colision
	je limpiar_tecla                 ; si  no ocurre una colisión
	inc dx
	jmp limpiar_tecla
flecha_der:
	inc dx
	call revisar_colision
	je limpiar_tecla                ; si  no ocurre una colisión
	dec dx
	jmp limpiar_tecla
flecha_arriba:
	mov bl, al
	inc ax
	inc ax
	test al, 00000111b           ; chequear el "overflow"
	jnz nf                       ; si no hay desvordamiento
	sub al, 8
nf: call revisar_colision
	je limpiar_tecla                ; si  no ocurre una colisión
	mov al, bl
limpiar_tecla:
	call imp_bloque
	push ax
	xor ah, ah                   ; limpia el registro de tecla
	int 0x16
	pop ax
no_tecla:
	pop cx
	loop esperar

	call borrar_bloque
	inc dh                       ; incrementa la fila
	call revisar_colision
	je lp                        ; si  no ocuurre una colision
	dec dh
	call imp_bloque
	call comp_llenado
	jmp nuevo_bloque

; ------------------------------------------------------------------------------

set_and_write:
	mov ah, 2                    ; asigna el cursor
	int 0x10
	mov ax, 0x0920               ; escribe las cajas
	int 0x10
	ret

set_and_read:
	mov ah, 2                    ; asigna la posicion del cursor
	int 0x10
	mov ah, 8                    ; Leer caracter y atributo, BH = 0
	int 0x10                     ; resultado en AX
	ret

; ------------------------------------------------------------------------------

;--------------------------------  MACRO #6  -------------------------------------------------------------
;Macro-6: _cambio de fila
;   cambia la fila actual por la fila de arriba
;-----------------------------------------------------------------------------------------------------------------

; DH = cambio de fila
%macro cambio_fila_actual 0
	pusha                          
 	mov dl, interior_primer_col
 	mov cx, ancho_interior
cf_aa:
	push cx
	dec dh                          ; decrementa la fila
	call set_and_read
	inc dh                          ; incrementa la fila
	mov bl, ah                      ; color de AH a BL
	mov cl, 1
	call set_and_write
	inc dx                          ; siguiente columna
	pop cx
	loop cf_aa
	popa
%endmacro

comp_llenado:
	pusha
	mov dh, 21                       ; comienza en la fila 21
fila_siguiente:
	dec dh                           ; decrementa la fila
	jz cf_done                       ; finaliza en fila 0
	xor bx, bx
	mov cx, ancho_interior
	mov dl, interior_primer_col          ; comienza en la primera columna interna
cf_loop:
	call set_and_read
	shr ah, 4                        ; rota para obtener el color de fondo
	jz cf_is_zero                    ; salta si el color de fondo es cero
	inc bx                           ; incrementa contador
	inc dx                           ; sigue con la otra columna
cf_is_zero:
	loop cf_loop
	cmp bl, ancho_interior              ; si contador es 12 se tiene una fila llena
	jne fila_siguiente
remplazar_fila_siguiente:                    ; reemplaza filas actuales por las de arriba
	cambio_fila_actual
	dec dh                           ; reemplaza filas de arriba
	jnz remplazar_fila_siguiente
	call comp_llenado                ; chequea por filas llenas
cf_done:
	popa
	ret

borrar_bloque:
	xor bx, bx
	jmp imp_bloque_no_color
imp_bloque:  ; al = 0AAAARR0
	mov bl, al                   ; selecciona el color correcto
	shr bl, 3
	inc bx
	shl bl, 4
imp_bloque_no_color:
	inc bx                       ; asigna el bit menos significativo
	mov di, bx
	jmp revisar_colision_main
	; BL = color of brick
	; DX = position (DH = row), AL = brick offset
	; return: flag
revisar_colision:
	mov di, 0
revisar_colision_main:            ; DI = 1 -> chequear, 0 -> imprimir
	pusha
	xor bx, bx                   ; Cargar el ladrillo en AX
	mov bl, al
	mov ax, word [bloques + bx]

	xor bx, bx                   ; BH = Número de página, BL = Contador de colisión
	mov cx, 4
cc:
	push cx
	mov cl, 4
zz:
	test ah, 10000000b
	jz is_zero

	push ax
	or di, di
	jz ee                        ; chequeo de colisiones
	pusha                        ; pinta con el color de di
	mov bx, di                   ; en posicion dx
	xor al, al
	mov cx, 1
	call set_and_write
	popa
	jmp is_zero_a
ee:
	call set_and_read
	shr ah, 4                    ; rota para obtener el color de fondo
	jz is_zero_a                 ; jsalta si el fondo es cero
	inc bx
is_zero_a:
	pop ax

is_zero:
	shl ax, 1                    ; pasa al siguiente bit en mascara de bloque
	inc dx                       ; se mueve a la siguiente columna
	loop zz
	sub dl, 4                    ; reinicia columna
	inc dh                       ; se mueve a siguiente fila
	pop cx
	loop cc
	or bl, bl                    ; bl != 0 -> colision
	popa
	ret

;****************************************************************************************
;*                                  Ladrillos                                                                                                                                                   *
;****************************************************************************************
bloques:
	;  en AL      en AH
	;  3era + 4ta  1er + 2da fila
	db 01000100b, 01000100b, 00000000b, 11110000b
	db 01000100b, 01000100b, 00000000b, 11110000b
	db 01100000b, 00100010b, 00000000b, 11100010b
	db 01000000b, 01100100b, 00000000b, 10001110b
	db 01100000b, 01000100b, 00000000b, 00101110b
	db 00100000b, 01100010b, 00000000b, 11101000b
	db 00000000b, 01100110b, 00000000b, 01100110b
	db 00000000b, 01100110b, 00000000b, 01100110b
	db 00000000b, 11000110b, 01000000b, 00100110b
	db 00000000b, 11000110b, 01000000b, 00100110b
	db 00000000b, 01001110b, 01000000b, 01001100b
	db 00000000b, 11100100b, 10000000b, 10001100b
	db 00000000b, 01101100b, 01000000b, 10001100b
	db 00000000b, 01101100b, 01000000b, 10001100b
;*****************************************************************************************

%ifndef DEBUG
;  se necesita una entrada de tabla de partición ficticia para la portátil
times 446-($-$$) db 0
	db 0x80                   ; arranque
    db 0x00, 0x01, 0x00       ; Iniciar Dirección CHS 
    db 0x17                   ; Tipo de partición
    db 0x00, 0x02, 0x00       ; finalizar dirección CHS 
    db 0x00, 0x00, 0x00, 0x00 ; LBA
    db 0x02, 0x00, 0x00, 0x00 ; numero de sectores

; Al final necesitamos la firma del sector de arranque.
times 510-($-$$) db 0
	db 0x55
	db 0xaa
%endif
