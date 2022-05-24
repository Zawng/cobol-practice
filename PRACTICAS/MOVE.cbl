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
           05 WS-NUM1    PIC 99.
           05 WS-NUM2    PIC 99.
           05 WS-NUM3    PIC 99.
           05 WS-NUM4    PIC 99.
           05 WS-NUM5    PIC 99.
           05 WS-NUM6    PIC 99.

       PROCEDURE DIVISION.
       010-MAIN.
           MOVE 10 TO WS-NUM1 WS-NUM2 WS-NUM3 WS-NUM4 WS-NUM5 WS-NUM6.  Mueve un valor a diferentes variables.
           DISPLAY WS-NUM1 WS-NUM2 WS-NUM3 WS-NUM4 WS-NUM5 WS-NUM6.
           PERFORM 0100-STOP.

       0100-STOP.
           STOP RUN.
           END PROGRAM BASE.