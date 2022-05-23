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
          05 WS-NOMBRE           PIC X(11) VALUE 'EDWIN PÁEZ'.

       01 WS-CONSTANTES.
          05 CN-TITULO            PIC X(11) VALUE 'CALCULADORA'.

       PROCEDURE DIVISION.
       010-MAIN.
           DISPLAY WS-NOMBRE SPACE CN-TITULO.
           PERFORM 0100-STOP.

       0100-STOP.
           STOP RUN.
           END PROGRAM BASE.