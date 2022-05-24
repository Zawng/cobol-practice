      ******************************************************************
      * Author: Edwin Páez
      * Date: 16-05-22
      * Purpose: Practice COBOL
      ******************************************************************
      * Sección #1 - Obligatoria
       IDENTIFICATION DIVISION.

      * Sección #2
       ENVIRONMENT DIVISION.

      * Sección #3
       DATA DIVISION.

      * Sección 4
       PROCEDURE DIVISION.                                              Sección de procedimientos, lógica
       010-MAIN.                                                           Rutinas, párrafos, secciones
           PERFORM 020-HEADER.                                             Ejecuta rutinas o ciclos
           PERFORM 0100-STOP.
           GO TO 020-HEADER.                                               Va al párrafo y no vuelve a su posición base

       020-HEADER.
           DISPLAY WSC-ASTERISK.
           DISPLAY WSC-TITLE.
           DISPLAY WSC-ASTERISK.

       0100-STOP.
           STOP RUN.
       END PROGRAM TEST.