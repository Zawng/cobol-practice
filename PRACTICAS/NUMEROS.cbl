      ******************************************************************
      * Author: EDWIN PÁEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                BASE.
       AUTHOR.                    EDWIN-PAEZ.
       DATE-WRITTEN.              16/05/22.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
           05 WS-ENTEROS          PIC S9(5).                            S- Signo, 9- Numérico, V- Punto.
           05 WS-DECIMALES        PIC S9(2)V9(2).
           05 WS-RESULTADO        PIC S9(2)V9(2).
           05 WS-DECIMAL-CERO     PIC ZZZZV99 VALUE 0050.75.            Z, Si es 0 no se muestran
           05 WS-DECIMAL-MAL      PIC 9999.99 VALUE 0050.75.            Solo sirve para mostrar valores en los displays.
           05 WS-COMPUTE          PIC 9(10) VALUE ZEROS.

       PROCEDURE DIVISION.
       010-MAIN.
           MOVE -10.14 TO WS-DECIMALES.
           MOVE 10140 TO WS-ENTEROS.
           ADD WS-DECIMALES TO WS-ENTEROS GIVING WS-RESULTADO.
           COMPUTE WS-COMPUTE = 10 * 10 + 10 + 10
           DISPLAY WS-RESULTADO.
           DISPLAY WS-DECIMAL-CERO.
           DISPLAY WS-DECIMAL-MAL.
           DISPLAY WS-COMPUTE.
           PERFORM 0100-STOP.

       0100-STOP.
           STOP RUN.
           END PROGRAM BASE.