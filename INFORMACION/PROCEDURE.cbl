      ******************************************************************
      * Author: Edwin Páez
      * Date: 16-05-22
      * Purpose: Practice COBOL
      ******************************************************************
      * Seccion #1 - Obligatoria
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.

      * Seccion #2
       ENVIRONMENT DIVISION.

      * Seccion #3
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 NUM1 PIC 9(5).
       77 NUM2 REDEFINES NUM1 PIC X(7).                                 Convierte de numero a texto, no se puede en nivel 1, abajo de la original

      * Seccion 4
       PROCEDURE DIVISION.                                              Seccion de procedimientos, logica
       010-MAIN.                                                           Rutinas, parrafos, secciones
           PERFORM 020-HEADER.                                             Ejecuta rutinas o ciclos
           PERFORM 0100-STOP.

           GO TO 020-HEADER.                                               Va al parrafo y no vuelve a su posicion base

       020-HEADER.
           MOVE 5555 TO NUM1
           DISPLAY NUM1
           DISPLAY NUM2.

       0100-STOP.
           STOP RUN.
       END PROGRAM TEST.
