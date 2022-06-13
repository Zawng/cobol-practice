
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * ENTRADA:
      * [ ] RECIBIR DOS NUMEROS
      *     DIGITE UN NUMERO POSITIVO (12ENT).(2DEC)
      *     DIGITE UN NUMERO NEGATIVO (12ENT).(2DEC)
      * SALIDA:
      * [ ] NUMERO POSITIVO: MOSTRAR NUMERO TAL CUAL
      * [ ] NUMERO NEGATIVO: MOSTRAR NUMERO TAL CUAL
      * [ ] 1) MÁSCARA
      * [ ] 2) MÁSCARA
      * [ ] 3) MÁSCARA
      * [ ] 4) MÁSCARA
      * [ ] 5) MÁSCARA
      * [ ] 6) MÁSCARA
      * [ ] 7) MÁSCARA SIGNOS
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
