
      ******************************************************************
      * Author: EDWIN PAEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PERFORM-THRU.
       AUTHOR.                    EDWIN-PAEZ.
       DATE-WRITTEN.              16/05/22.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
          05 WS-NUMBER            PIC S(10).

       PROCEDURE DIVISION.
       010-MAIN.
           PERFORM ITERATOR UNTIL WS-NUMBER = 0.                        Itera hasta que una variable sea igual que
           PERFORM ITERATOR VARYING WS-NUMBER FROM 10 BY -1 UNTIL       Itera definiendo un inicio y un final, modificando su flujo
           WS-NUMBER < 0.
           PERFORM ITERATOR THRU DISPLAYS.                              Ejecuta dos parrafos
           PERFORM ITERATOR 3 TIMES.                                    Ejecuta 3 veces el parrafo
           PERFORM 0100-STOP.

       ITERATOR.
           DISPLAY WS-NUMBER.

       0100-STOP.
           STOP RUN.
           END PROGRAM PERFORM-THRU.