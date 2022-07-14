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
      *                           ENTRADAS                             *
      *----------------------------------------------------------------*
       01  WS-OPERA                  PIC A VALUE SPACES.
           88 OPE-COMPRA             VALUE 'C' 'c'.
           88 OPE-VENTA              VALUE 'V' 'v'.

       01  WS-DIVISA                 PIC 9 VALUE ZEROS.
           88 DOLAR                  VALUE 1.
           88 EURO                   VALUE 2.
           88 LIBRA                  VALUE 3.
           88 YEN                    VALUE 4.
           88 DOLCAN                 VALUE 5.

       01  WS-CANTI                  PIC 9(5) VALUE ZEROS.
       01  WS-FORPAG-COM             PIC 9 VALUE ZEROS.
           88 EFECTI-COM             VALUE 1.
           88 DEBITO                 VALUE 2.
           88 CREDITO                VALUE 3.

       01  WS-FORPAG-VEN             PIC 9 VALUE ZEROS.
           88 EFECTI-VEN             VALUE 1.
           88 CHEQUE                 VALUE 2.
           88 TRASFER                VALUE 3.

       01  WS-REALIZA-OPE            PIC A VALUE SPACES.
           88 SI-REALIZA             VALUE 'S' 's'.
           88 NO-REALIZA             VALUE 'N' 'n'.

       01  WS-OPC                    PIC 9 VALUE ZEROS.

      *----------------------------------------------------------------*
      *                           PROCESOS                             *
      *----------------------------------------------------------------*
       01  TABLA-MONEY.
           02 TAB-OPE                OCCURS 2 TIMES.
             05 TAB-DIVI             OCCURS 5 TIMES.
               10 TAB-FORMA          OCCURS 3 TIMES.
                 15 TAB-CANTIDA      PIC 9(7) VALUE ZEROS.
                 15 TAB-VALDIVI      PIC 9(5)V9(02) VALUE ZEROS.
                 15 TAB-TOTDIVI      PIC 9(12)V9(02) VALUE ZEROS.

      * O - OPERACION: 1 = COMPRAS, 2 = VENTAS
       01  O                         PIC 9 VALUE ZEROS.
      * D- DIVISAS: 1: DOLAR, 2: EUROS, 3: LIBRAS, 4: YENES, 
      *             5 CANADIENSES.
       01  D                         PIC 9 VALUE ZEROS.
      * F - FORMA: (COMPRAS: 1 EFECTIVO, 2 DEBITO, 3 CREDITO)
      *            (VENTAS:  1 EFECTIVO, 2 CHEQUE, 3 TRANSFERENCIA )
       01  F                         PIC 9 VALUE ZEROS.

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
           PERFORM MENU-PRINCIPAL UNTIL WS-OPC = 3
           PERFORM 3000-FINAL.

       MENU-PRINCIPAL.
           DISPLAY CLEAR-SCREEN
           DISPLAY 'CASA DE CAMBIO MONEY'   LINE 02 POSITION 29
                   'MENU PRINCIPAL'         LINE 05 POSITION 33
                   '1. OPERACIONES'         LINE 07 POSITION 33
                   '2. CIERRE'              LINE 09 POSITION 33
                   '3. SALIR'               LINE 11 POSITION 33
                   'QUE OPCION DESEA?: '    LINE 13 POSITION 36
           MOVE 0 TO WS-OPC
           PERFORM UNTIL WS-OPC > 0 AND < 4
               ACCEPT WS-OPC LINE 13 POSITION 56
           END-PERFORM
           EVALUATE WS-OPC
               WHEN 1 PERFORM 1-OPERACIONES
               WHEN 2 PERFORM 2-CIERRE
           END-EVALUATE.

       1-OPERACIONES.
           DISPLAY CLEAR-SCREEN
           PERFORM 1-01-OPERACION
           PERFORM 1-02-DIVISAS
           PERFORM 1-03-CANTIDAD
           PERFORM 1-04-FORMA-PAGO
           PERFORM 1-05-AVERIGUO-DIVISA
           PERFORM 1-06-MOSTRAR-OPERACION
           IF SI-REALIZA
               PERFORM 1-07-GUARDAR-OPERACION.
           END-IF.

       1-01-OPERACION.
           DISPLAY 'OPERACION A REALIZAR (C: COMPRA / V: VENTA):'
                                            LINE 02 POSITION 01
           MOVE SPACES TO WS-OPERA
           PERFORM UNTIL OPE-COMPRA OR OPE-VENTA
               ACCEPT WS-OPERA LINE 02 POSITION 50
           END-PERFORM.
           
       1-02-DIVISAS.
           DISPLAY 'QUE DIVISA DESEA?: '    LINE 04 POSITION 15
                   '1) DOLAR           '    LINE 05 POSITION 05
                   '2) EURO            '    LINE 06 POSITION 05
                   '3) LIBRA           '    LINE 07 POSITION 05
                   '4) YEN             '    LINE 08 POSITION 05
                   '5) DOLAR CANADIENSE'    LINE 09 POSITION 05
                   'QUE DIVISA DESEA?:'     LINE 10 POSITION 05
           MOVE ZEROS TO WS-DIVISA
           PERFORM UNTIL WS-DIVISA > 0 AND < 6
               ACCEPT WS-OPERA              LINE 10 POSITION 25
           END-PERFORM.

       1-03-CANTIDAD. 
           DISPLAY 'CANTIDAD DE LA DIVISA?: '
                                            LINE 12 POSITION 01
           MOVE ZEROS TO WS-CANTI
           PERFORM UNTIL WS-CANTI > 0 
               ACCEPT WS-CANTI              LINE 12 POSITION 25
           END-PERFORM.

       1-04-FORMA-PAGO.

       999-ENTER.
           DISPLAY ' <OPRIMA ENTER> '       LINE 24 POSITION 33
           ACCEPT WS-ENTER                  LINE 24 POSITION 50.

       999-MENSAJE-ERROR.
           STRING 'ERROR EN TAMANO EN LA VARIABLE : '
               WS-ERROR DELIMITED BY SIZE
               INTO WS-MENSAJE-ERROR
           END-STRING
           DISPLAY WS-MENSAJE-ERROR         LINE 24 POSITION 05
           ACCEPT WS-ENTER                  LINE 24 POSITION 67
           DISPLAY WS-BLANCOS               LINE 24 POSITION 01.

       3000-FINAL.
           STOP RUN.
