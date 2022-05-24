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

       01 EDAD PIC 999.
           88 JOVEN   VALUE 1 THRU 40.
           88 MADURO  VALUE 41 THRU 65.
           88 ANCIANO VALUE 66 THRU 100.

       PROCEDURE DIVISION.
       010-MAIN.
           DISPLAY "INGRESA TU EDAD.".
           ACCEPT EDAD.
           IF JOVEN
               DISPLAY "ERES JOVEN.".
           IF MADURO
               DISPLAY "ERES MADURO".
           IF ANCIANO
               DISPLAY "ERES ANCIANO"
           PERFORM 0100-STOP.

       0100-STOP.
           STOP RUN.
           END PROGRAM BASE.