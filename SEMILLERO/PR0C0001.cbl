
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * ENTRADA:
      * [ ] RECIBIR 5 NUMEROS de 5 DIG
      * SUMA NUMEROS PARES:
      * SUMA NUMEROS IMPARES:
      * SALIDA:
      ******************************************************************

      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                             NO0C0004.
       AUTHOR.                                 NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                           BBVA.
       DATE-WRITTEN.                           16-JUN-22.

      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           02 WS-CENTURY           PIC 9(5).
       PROCEDURE DIVISION.
           INITIALIZE WS-CENTURY
           ACCEPT WS-CENTURY FROM DAY
           DISPLAY WS-CENTURY
           STOP RUN.
