
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       BASE.
       AUTHOR.                           NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     05-JUL-22.

      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
       COPY './RUTINAS/VARFECHAS.CPY'.

      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPCION                 PIC A(01) VALUE SPACES.

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
           PERFORM 2001-FECHAS
           PERFORM 3000-FINAL.

      *----------------------------------------------------------------*
      * OBTENER LA FECHA Y LA HORA DEL SISTEMA
      *----------------------------------------------------------------*
       COPY './RUTINAS/PARFECHAS.CPY'.

      * TOTAL A PAGAR

       2090-OPCION-NO-ENCONTRADA.
           DISPLAY 'ERROR: '
                                         LINE 12 POSITION 50
           DISPLAY WS-MENSAJE-ERROR      LINE 12 POSITION 57.

       2095-OPCION.
           DISPLAY 'PRESIONE UNA TECLA: '
                                         LINE 14 POSITION 50
           ACCEPT WS-OPCION              LINE 14 POSITION 70. 

       3000-FINAL.
           STOP RUN.
