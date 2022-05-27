      ******************************************************************
      * Author: EDWIN PAEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************

      *----------------------------------------------------------------*
      *                      IDENTIFICATION DIVISION                   *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                BASE.
       AUTHOR.                    EDWIN-PAEZ.
       INSTALLATION               NOVATEC.
       DATE-WRITTEN.              16-05-22.
       DATE-COMPILED.
       REMARKS.                   BASE DE PROYECTO COBOL.

      *----------------------------------------------------------------*
      *                        ENVIRONMENT DIVISION                    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.           NOVATEC-IBM.
       OBJECT-COMPUTER.           NOVATEC-IBM.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *----------------------------------------------------------------*
      *                          DATA DIVISION                         *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       LINKAGE SECTION.

      *----------------------------------------------------------------*
      *                         PROCEDURE DIVISION                     *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-HOLA
       PERFORM 3000-FINAL.

       2000-PROCESOS.
       2001-HOLA.
           DISPLAY "HOLA MUNDO".

       3000-FINAL.
           STOP RUN.
           END PROGRAM BASE.
