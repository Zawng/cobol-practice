      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      * OBJETIVO: SIMULAR LOS MOVIMIENTOS DE UNA CAJA DE CAMBIO
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       NO1CMONY.
       AUTHOR.                           NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     07-JUL-22.

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
       01  WS-ENTER                  PIC A VALUE SPACES.
       01  WS-ERROR                  PIC X(30) VALUE SPACES.
       01  WS-MENSAJE-ERROR          PIC X(60) VALUE SPACES.
       01  WS-BLANCOS                PIC X(80) VALUE SPACES.

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
           PERFORM 2001-FECHAS
           PERFORM 1001-TEST
           PERFORM 3000-FINAL.

      *----------------------------------------------------------------*
      * OBTENER LA FECHA Y LA HORA DEL SISTEMA
      *----------------------------------------------------------------*
       COPY './RUTINAS/PARFECHAS.CPY'.

      * TEST 
       1001-TEST.
           DISPLAY 'HELLO WORLD'         LINE 12 POSITION 01
           PERFORM 999-MENSAJE-ERROR.

       999-ENTER.
           DISPLAY ' <OPRIMA ENTER> '       LINE 24 POSITION 33
           ACCEPT WS-ENTER                  LINE 24 POSITION 50.

       999-MENSAJE-ERROR.
           STRING 'ERROR EN TAMANO EN LA VARIABLE : '
               WS-ERROR DELIMITED BY SIZE
               INTO WS-MENSAJE-ERROR
           END-STRING
           DISPLAY WS-MENSAJE-ERROR LINE 24 POSITION 05
           ACCEPT WS-ENTER          LINE 24 POSITION 67
           DISPLAY WS-BLANCOS       LINE 24 POSITION 01.

       3000-FINAL.
           STOP RUN.
