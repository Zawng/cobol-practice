      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      * OBJETIVO: SIMULAR LOS MOVIMIENTOS DE UNA CAJA DE CAMBIO
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       NO1CMONY.
       AUTHOR.                           NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     13-JUL-22.

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
      * ENTRADAS
      *----------------------------------------------------------------*
       01  WS-OPERA                  PIC A VALUE SPACES.
           88 OPE-COMPRA             PIC 'C' 'c'.
           88 OPE-VENTA              PIC 'V' 'v'.

       01  WS-DIVISA                 PIC 9 VALUE ZEROS.
           88 DOLAR                  VALUE 1.
           88 EURO                   VALUE 2.
           88 LIBRA                  VALUE 3.
           88 YEN                    VALUE 4.
           88 DOLCAN                 VALUE 5.

       01  WS-CANTI                  PIC 9(5) VALUE ZEROS.
       01  WS-FORPAG-COMPRAS         PIC 9 VALUE ZEROS.
           88 


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
