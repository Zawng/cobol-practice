      ******************************************************************
      * Author: Edwin Páez
      * Date: 16-05-22
      * Purpose: Practice COBOL
      ******************************************************************
      * Sección #1 - Obligatoria
      IDENTIFICATION DIVISION.

      * Sección #3
       DATA DIVISION.                                                   Sección de datos

       FILE SECTION.                                                    Variables que componen los registros de todos los archivos - INPUT-OUTPUT

      * Recordar que los nombres de las variables no deben pasar los 31 caracteres de longitud
      * X => Alfanumérico
      * A => Alfabético
      * 9 => Números
      * V => Punto virtual
      * S => Signo
      * P => Decimal asumido

       WORKING-STORAGE SECTION.                                         Declararación de variables que no tienen nada que ver con archivos
       77 WSS-FIN                       PIC X(03) VALUE '   '.             Booleanos
           88 WSS-SI                              VALUE 'FIN'.
       01 WSV-VARIABLES.                                                   Variables
           05 WSV-NAME                  PIC X(05) VALUE 'EDWIN'.
       01 WSC-CONSTANTES.                                                  Constantes
           05 WSC-TITLE.
               010 FILLER               PIC X(33) VALUE ALL '*'.
               010 WSC-NAME             PIC X(13) VALUE ' CALCULADORA '.
               010 FILLER               PIC X(34) VALUE ALL '*'.
           05 WSC-ASTERISK.
               010 FILLER               PIC X(80) VALUE ALL '*'.

       LINKAGE SECTION.                                                 Variables que enlazarán al progama principal

       COMMUNICATION SECTION.                                           Se usa para la comunicación entre dos programas que se ejecutan simultáneamente

       SCREEN SECTION.                                                  Se usan los atributos a usar en pantallas
