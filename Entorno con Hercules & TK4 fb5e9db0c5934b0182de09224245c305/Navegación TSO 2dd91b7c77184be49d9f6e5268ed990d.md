# Navegación TSO

**Navegación por el entorno**

| ATAJO | FUNCIÓN |
| --- | --- |
| F2 | Nueva ventana, máximo 16 |
| F3 | Volver o salir |
| F9 | Cambiar entre ventanas |
| F7 | Desplazar hacia arriba |
| F8 | Desplazar hacia abajo |
| F10 | Desplazar hacia derecha |
| F11 | Desplazar hacia izquierda |
| F12 | Historial de comandos |
| TAB | Salto de inputs |
| I | Insertar texto |
| D | Eliminar |
| R | Repetir |
| )N | Indentar a la derecha |
| (N | Indentar a la izquierda |
| FIND “argument” | Buscar palabra |
| C “argument” “argument” | Reemplazar palabras |
| COLS | Muestra las columnas |
| C99999 + command CRE HELLO | Copia las líneas del programa y crea un archivo con ello llamado HELLO |
| E | Editar |
| B | Browse |
| UNDO | Deshacer lo hecho |
| SORT CHA | Organiza por últimos cambios |
| C “argument” “argument” ALL | Reemplaza un texto por otro en todas los encuentros |

Para hacer funcionalidades de bloques, debemos colocar la fórmula NN hasta NN, ejemplo:

```bash
DD0001    
000002    
000003    
000004    
00DD05    
```

**Opciones de SCROLL**

| MODO | COMPORTAMIENTO |
| --- | --- |
| CS | Cursor “desplazamiento corto” |
| HF | Half “desplazamiento a mitad de consola” |
| PG | Page “desplazamiento a la próxima página” |

**Ingresar a la línea de comandos del entorno MVS - Transferencia de archivos**

Navegación en la línea de comandos

| COMANDO | FUNCIÓN |
| --- | --- |
| V | Modo visual |
| B | Buscar |
| CAN | Cancelar |
| REF | Refrescar el programa |
| P | Purgar en el spool |

Para purgar todos los JOBS en el MVS podemos hacerlo desde HERCULES usando

/$PJ1-9999