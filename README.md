# Descripción del Juego

El juego está compuesto por diferentes ladrillos los cuales no se puede impedir su caída, pero sí su rotación (0°, 90°, 180°, 270°). El jugador puede controlar la posición y lugar en la que cae el bloque mediante las direccionales del teclado y en caso de presionar cualquier otra tecla, la caída del ladrillo se acelera.

En el isntante en que una fila es completada, ésta se elimina y las demás piezas decienden una posición por lo que permite liberar espacio. Si se llega a la fila más alta, entonces el juego finaliza.

## Instalación

1) Tener instalado en Linux nasm, el emulador qemu
2) Abrir el terminal de linux (Ctrl+Alt+T)
3) Ingresar a la carpeta en la que se encuentra el archivo .asm (cd ProyectoDefinitivo)
4) Escribir en el terminal: nasm -f bin juego.asm -o juego.img
5) Escribir en el terminal: qemu-system-i386 -drive file=tetros.img,index=0,media=disk,format=raw




