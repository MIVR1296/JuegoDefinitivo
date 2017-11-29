;*******************************************************************************************
;*                                    Juego Tetris                                         *
;*******************************************************************************************
	org 7c00h ;Con ORG se le indica al programa que debe iniciar en esta posición

;*******************************************************************************************
;*                                    Creación de macros                                   *
;*******************************************************************************************

;--------------------------------  MACRO #1  -----------------------------------------------
;Macro-1: print_reg .
;	Recibe 1 parametro de entrada:
;		%1 es la direccion del registro a imprimir
;		
;--------------------------------------------------------------------------------------------

%macro print_reg 1 
	mov dx, %1 ; mueva el parámetro de entrada al registro de datos "dx"
	mov cx, 16 ; mueva el valor al registro contador "cx"
print_reg_loop:
	push cx ; La instrucción PUSH decrementa en dos el valor deL registro apuntador de pila
	;y posteriormente transfiere el contenido del operando fuente a la nueva dirección resultante en el registro recién modificado.
	mov al, '0' ; mueve '0' a la parte baja del registro acomulador
	test dh, 10000000b ;
	
	;_____________________________________________________________________
        ;TEST: Realiza una conjunción, bit por bit, de los operandos, 
        ;a diferencia del "and"  no coloca el resultado en el operando destino, 
        ; sino que solo tiene efecto sobre el estado de las banderas
        ;____________________________________________________________________
       
	jz print_reg_do ; Si la vandera es cero salta a la etiqueta print_reg_do
	mov al, '1' ;  mueve '1' a la parte baja del registro acomulador
print_reg_do:
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
	loop print_reg_loop        
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
	popa      ; extrae de la pila los registros básicos (orden ibversi ak pusha
%endmacro
;---------------------------------------------------------------------------------------------------------
;--------------------------------  MACRO #3  -------------------------------------------------------------
;Macro-3: select_brick
;	Recibe 1 parametro de entrada:	
;       Selección de un bloque de manera aleatoria
;---------------------------------------------------------------------------------------------------------
%macro select_brick 0
	mov ah, 2  ;  con la "int 0x1a" se obtiene la hora actual del sistema
	int 0x1a
	mov al, byte [seed_value]
	xor ax, dx
	mov bl, 31
	mul bx
	inc ax ; La instrucción suma 1 al operando destino y guarda el resultado en el mismo operando destino.
	mov byte [seed_value], al
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
;Macro-4: clear_screen
;	Recibe 1 parametro de entrada:	
;        Establece el modo de video y oculta el cursor
;---------------------------------------------------------------------------------------------------------
; Establece el modo de video y oculta el cursor
%macro clear_screen 0
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
field_left_col:  equ 13
field_width:     equ 14
inner_width:     equ 12
inner_first_col: equ 14
start_row_col:   equ 0x0412
;*********************************************************************************************************

%macro init_screen 0
	clear_screen
	mov dh, 3                        ; row
	mov cx, 18                       ; number of rows
ia: push cx
	inc dh                           ; increment row
	mov dl, field_left_col           ; set column
	mov cx, field_width              ; width of box
	mov bx, 0x78                     ; color
	call set_and_write
	cmp dh, 21                       ; don't remove last line
	je ib                            ; if last line jump
	inc dx                           ; increase column
	mov cx, inner_width              ; width of box
	xor bx, bx                       ; color
	call set_and_write
ib: pop cx
	loop ia
%endmacro

; ==============================================================================

delay:      equ 0x7f00
seed_value: equ 0x7f02

section .text

start_tetris:
	xor ax, ax
	mov ds, ax
	init_screen
new_brick:
	mov byte [delay], 100            ; 3 * 100 = 300ms
	select_brick                     ; returns the selected brick in AL
	mov dx, start_row_col            ; start at row 4 and col 38
lp:
	call check_collision
	jne $                            ; collision -> game over
	call print_brick

wait_or_keyboard:
	xor cx, cx
	mov cl, byte [delay]
wait_a:
	push cx
	sleep 3000                       ; wait 3ms

	push ax
	mov ah, 1                    ; check for keystroke; AX modified
	int 0x16                     ; http://www.ctyme.com/intr/rb-1755.htm
	mov cx, ax
	pop ax
	jz no_key                    ; no keystroke
	call clear_brick
                                 ; 4b left, 48 up, 4d right, 50 down
	cmp ch, 0x4b                 ; left arrow
	je left_arrow                ; http://stackoverflow.com/questions/16939449/how-to-detect-arrow-keys-in-assembly
	cmp ch, 0x48                 ; up arrow
	je up_arrow
	cmp ch, 0x4d
	je right_arrow

	mov byte [delay], 10         ; every other key is fast down
	jmp clear_keys
left_arrow:
	dec dx
	call check_collision
	je clear_keys                 ; no collision
	inc dx
	jmp clear_keys
right_arrow:
	inc dx
	call check_collision
	je clear_keys                ; no collision
	dec dx
	jmp clear_keys
up_arrow:
	mov bl, al
	inc ax
	inc ax
	test al, 00000111b           ; check for overflow
	jnz nf                       ; no overflow
	sub al, 8
nf: call check_collision
	je clear_keys                ; no collision
	mov al, bl
clear_keys:
	call print_brick
	push ax
	xor ah, ah                   ; remove key from buffer
	int 0x16
	pop ax
no_key:
	pop cx
	loop wait_a

	call clear_brick
	inc dh                       ; increase row
	call check_collision
	je lp                        ; no collision
	dec dh
	call print_brick
	call check_filled
	jmp new_brick

; ------------------------------------------------------------------------------

set_and_write:
	mov ah, 2                    ; set cursor
	int 0x10
	mov ax, 0x0920               ; write boxes
	int 0x10
	ret

set_and_read:
	mov ah, 2                    ; set cursor position
	int 0x10
	mov ah, 8                    ; read character and attribute, BH = 0
	int 0x10                     ; result in AX
	ret

; ------------------------------------------------------------------------------

; DH = current row
%macro replace_current_row 0
	pusha                           ; replace current row with row above
 	mov dl, inner_first_col
 	mov cx, inner_width
cf_aa:
	push cx
	dec dh                          ; decrement row
	call set_and_read
	inc dh                          ; increment row
	mov bl, ah                      ; color from AH to BL
	mov cl, 1
	call set_and_write
	inc dx                          ; next column
	pop cx
	loop cf_aa
	popa
%endmacro

check_filled:
	pusha
	mov dh, 21                       ; start at row 21
next_row:
	dec dh                           ; decrement row
	jz cf_done                       ; at row 0 we are done
	xor bx, bx
	mov cx, inner_width
	mov dl, inner_first_col          ; start at first inner column
cf_loop:
	call set_and_read
	shr ah, 4                        ; rotate to get background color in AH
	jz cf_is_zero                    ; jmp if background color is 0
	inc bx                           ; increment counter
	inc dx                           ; go to next column
cf_is_zero:
	loop cf_loop
	cmp bl, inner_width              ; if counter is 12 full we found a full row
	jne next_row
replace_next_row:                    ; replace current row with rows above
	replace_current_row
	dec dh                           ; replace row above ... and so on
	jnz replace_next_row
	call check_filled                ; check for other full rows
cf_done:
	popa
	ret

clear_brick:
	xor bx, bx
	jmp print_brick_no_color
print_brick:  ; al = 0AAAARR0
	mov bl, al                   ; select the right color
	shr bl, 3
	inc bx
	shl bl, 4
print_brick_no_color:
	inc bx                       ; set least significant bit
	mov di, bx
	jmp check_collision_main
	; BL = color of brick
	; DX = position (DH = row), AL = brick offset
	; return: flag
check_collision:
	mov di, 0
check_collision_main:            ; DI = 1 -> check, 0 -> print
	pusha
	xor bx, bx                   ; load the brick into AX
	mov bl, al
	mov ax, word [bricks + bx]

	xor bx, bx                   ; BH = page number, BL = collision counter
	mov cx, 4
cc:
	push cx
	mov cl, 4
zz:
	test ah, 10000000b
	jz is_zero

	push ax
	or di, di
	jz ee                        ; we just want to check for collisions
	pusha                        ; print space with color stored in DI
	mov bx, di                   ; at position in DX
	xor al, al
	mov cx, 1
	call set_and_write
	popa
	jmp is_zero_a
ee:
	call set_and_read
	shr ah, 4                    ; rotate to get background color in AH
	jz is_zero_a                 ; jmp if background color is 0
	inc bx
is_zero_a:
	pop ax

is_zero:
	shl ax, 1                    ; move to next bit in brick mask
	inc dx                       ; move to next column
	loop zz
	sub dl, 4                    ; reset column
	inc dh                       ; move to next row
	pop cx
	loop cc
	or bl, bl                    ; bl != 0 -> collision
	popa
	ret

; ==============================================================================

bricks:
	;  in AL      in AH
	;  3rd + 4th  1st + 2nd row
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

%ifndef DEBUG
; It seems that I need a dummy partition table entry for my notebook.
times 446-($-$$) db 0
	db 0x80                   ; bootable
    db 0x00, 0x01, 0x00       ; start CHS address
    db 0x17                   ; partition type
    db 0x00, 0x02, 0x00       ; end CHS address
    db 0x00, 0x00, 0x00, 0x00 ; LBA
    db 0x02, 0x00, 0x00, 0x00 ; number of sectors

; At the end we need the boot sector signature.
times 510-($-$$) db 0
	db 0x55
	db 0xaa
%endif
