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
           88 TRANSFER               VALUE 3.

       01  WS-REALIZA-OPE            PIC A VALUE SPACES.
           88 SI-REALIZA             VALUE 'S' 's'.
           88 NO-REALIZA             VALUE 'N' 'n'.
           
       01  RUT-NO6CDIVI              PIC X(08) VALUE 
                                     'NO6CDIVI'.
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

      * TABLA DE DIVISAS 
       01  LISTA-DIVISAS.
           02  FILLER PIC X(3) VALUE 'USD'.
           02  FILLER PIC X(3) VALUE 'EUR'.
           02  FILLER PIC X(3) VALUE 'GBP'.
           02  FILLER PIC X(3) VALUE 'JPY'.
           02  FILLER PIC X(3) VALUE 'CAD'.
       01  TABLA-DIVISAS             REDEFINES LISTA-DIVISAS.
           10 TAB-DIVI-SIG           OCCURS 5 TIMES PIC X(3).

       01  WS-CORRECTO               PIC X VALUE SPACES.
           88 SW-INCORRECTO          VALUE 'N'.
           88 SW-CORRECTO            VALUE 'S'.

       01  WS-VAL-OPE                PIC 9(12)V99 VALUE ZEROS.

      *----------------------------------------------------------------*
      * SALIDAS
      *----------------------------------------------------------------*
       01  WS-ENTER                  PIC A VALUE SPACES.
       01  WS-OPC                    PIC 9 VALUE ZEROS.
       01  WS-OPC2                   PIC 9 VALUE ZEROS.
       01  WS-ACUM-CANTI             PIC 9(7) VALUE ZEROS.
       01  WS-ACUM-TOTDI             PIC 9(12)V99 VALUE ZEROS.
       01  LI                        PIC 99 VALUE ZEROS.
       01  WS-VALI-DIVIS             PIC 9(5)V99 VALUE ZEROS.

      *----------------------------------------------------------------*
      * MASCARA
      *----------------------------------------------------------------*
       01  WS-MASCA-DIVI             PIC $$$,$$$.99.
       01  WS-MASCA-OPER             PIC $$$$,$$$,$$$,$$$.99.
       01  WS-MASCA-CANTI            PIC ZZZZ,ZZ9.99.

       COPY './COPYS/NOCODIVI.CPY'.

      * RUTINA DE FECHAS
       COPY './COPYS/VARFECHAS.CPY'.
       01  RUT-FECHAS                PIC X(08) VALUE 'NO6CFECH'. 

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
           INITIALIZE WS-FECHAS
           MOVE 1 TO WS-FORMATO
           PERFORM MENU-PRINCIPAL UNTIL WS-OPC = 3
           PERFORM 3000-FINAL.

       999-FECHAS.
           CALL RUT-FECHAS USING WS-FECHAS
           EVALUATE WS-RETORNO-FECHA
             WHEN '00' 
               DISPLAY 'FECHA DEL SISTEMA: '   LINE 01 POSITION 01
               DISPLAY WS-FORMATO-FECHA        LINE 01 POSITION 20
               DISPLAY 'HORA DEL SISTEMA: '    LINE 01 POSITION 54
               DISPLAY WS-FORMATO-HORA         LINE 01 POSITION 72
             WHEN '01'
               DISPLAY 'ERROR, FORMATO NO VALIDO'
                                            LINE 01 POSITION 01
           END-EVALUATE.

       MENU-PRINCIPAL.
           DISPLAY CLEAR-SCREEN
           PERFORM 999-FECHAS
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
               WHEN 2 
               MOVE 0 TO WS-OPC2
               PERFORM 2-CIERRE
           END-EVALUATE.

       1-OPERACIONES.
           DISPLAY CLEAR-SCREEN
           PERFORM 1-01-OPERACION
           PERFORM 1-02-DIVISAS
           PERFORM 1-03-CANTIDAD
           PERFORM 1-04-FORMA-PAGO
           PERFORM 1-05-AVERIGUO-DIVISA
           IF SW-CORRECTO
               PERFORM 1-06-MOSTRAR-OPERACION
               IF SI-REALIZA
                   PERFORM 1-07-GUARDAR-OPERACION
               END-IF
           END-IF.

       1-01-OPERACION.
           PERFORM 999-FECHAS
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
               ACCEPT WS-DIVISA             LINE 10 POSITION 25
           END-PERFORM.

       1-03-CANTIDAD. 
           DISPLAY 'CANTIDAD DE LA DIVISA?: '
                                            LINE 12 POSITION 01
           MOVE ZEROS TO WS-CANTI
           PERFORM UNTIL WS-CANTI > 0 
               ACCEPT WS-CANTI              LINE 12 POSITION 25
           END-PERFORM.

       1-04-FORMA-PAGO.
               DISPLAY 'FORMAS DE PAGO'     LINE 14 POSITION 15
               EVALUATE TRUE
               WHEN OPE-COMPRA
                   DISPLAY '1) EFECTIVO'       LINE 15 POSITION 05
                           '2) DEBITO'         LINE 16 POSITION 05
                           '3) CREDITO'        LINE 17 POSITION 05
                           'OPCION: '       LINE 18 POSITION 05
                   MOVE ZEROS TO WS-FORPAG-COM
                   PERFORM UNTIL WS-FORPAG-COM > 0 AND < 4
                       ACCEPT WS-FORPAG-COM LINE 18 POSITION 18
                   END-PERFORM
               WHEN OPE-VENTA
                   DISPLAY '1) EFECTIVO'       LINE 15 POSITION 05
                           '2) CHEQUE'         LINE 16 POSITION 05
                           '3) TRANSFERENCIA'  LINE 17 POSITION 05
                           'OPCION: '       LINE 18 POSITION 05
                   MOVE ZEROS TO WS-FORPAG-VEN
                   PERFORM UNTIL WS-FORPAG-VEN > 0 AND < 4
                       ACCEPT WS-FORPAG-VEN LINE 18 POSITION 18
                   END-PERFORM
           END-EVALUATE.

       1-05-AVERIGUO-DIVISA.
           INITIALIZE NOCODIVI
           SET SW-INCORRECTO TO TRUE
           EVALUATE TRUE
               WHEN OPE-COMPRA
                   MOVE 'C' TO CDIVI-E-OPERA
               WHEN OPE-VENTA
                   MOVE 'V' TO CDIVI-E-OPERA
           END-EVALUATE
           MOVE TAB-DIVI-SIG(WS-DIVISA) TO CDIVI-E-DIVISA
           CALL RUT-NO6CDIVI USING NOCODIVI
           EVALUATE CDIVI-R-CODRETO
               WHEN '00' SET SW-CORRECTO TO TRUE
               WHEN '01' DISPLAY 'ERROR, OPERACION NO INFORMADA O INEXIS
      -             'TENTE'                 LINE 24 POSITION 25   
                   ACCEPT WS-ENTER          LINE 24 POSITION 60
               WHEN '02' DISPLAY 'DIVISA NO INFORMADA O INEXISTENTE'
                                            LINE 24 POSITION 25   
                   ACCEPT WS-ENTER          LINE 24 POSITION 60
           END-EVALUATE.

       1-06-MOSTRAR-OPERACION.
           MOVE CDIVI-S-VALDIVI             TO WS-MASCA-DIVI
           DISPLAY 'EL VALOR DE LA DIVISA ES:' 
                                            LINE 19 POSITION 05
                   WS-MASCA-DIVI            LINE 19 POSITION 50
           MULTIPLY WS-CANTI BY CDIVI-S-VALDIVI GIVING WS-VAL-OPE 
                    ROUNDED
           MOVE WS-VAL-OPE TO WS-MASCA-OPER
           DISPLAY 'TOTAL A PAGAR:'         LINE 20 POSITION 30
                   WS-MASCA-OPER            LINE 20 POSITION 50
                   'REALIZA LA OPERACION (S/N)::'
                                            LINE 22 POSITION 35
           MOVE SPACES TO WS-REALIZA-OPE
           PERFORM UNTIL SI-REALIZA OR NO-REALIZA
               ACCEPT WS-REALIZA-OPE        LINE 22 POSITION 65
           END-PERFORM.

       1-07-GUARDAR-OPERACION.
           EVALUATE TRUE
               WHEN OPE-COMPRA
                   MOVE 1             TO O
                   MOVE WS-FORPAG-COM TO F
               WHEN OPE-VENTA
                   MOVE 2             TO O
                   MOVE WS-FORPAG-VEN TO F
           END-EVALUATE
           MOVE WS-DIVISA             TO D
           ADD WS-CANTI               TO TAB-CANTIDA(O D F)
           MOVE CDIVI-S-VALDIVI       TO TAB-VALDIVI(O D F)
           ADD WS-VAL-OPE             to TAB-TOTDIVI(O D F).

       2-CIERRE.
           PERFORM 2-01-MENU-CIERRE UNTIL WS-OPC2 = 3.

       2-01-MENU-CIERRE.
           DISPLAY CLEAR-SCREEN
           PERFORM 999-FECHAS
           DISPLAY 'CASA DE CAMBIO MONEY'   LINE 02 POSITION 29
                   'MENU DEL CIERRE DIARIO' LINE 05 POSITION 33
                   '1. COMPRAS Y VENTAS POR DIVISAS'
                                            LINE 07 POSITION 33
                   '2. FORMAS DE PAGO'      LINE 09 POSITION 33
                   '3. SALIR'               LINE 11 POSITION 33
                   'QUE OPCION DESEA?: '    LINE 13 POSITION 36
           MOVE ZEROS TO WS-OPC2
           PERFORM UNTIL WS-OPC2 > 0 AND < 4
               ACCEPT WS-OPC2 LINE 13 POSITION 56
           END-PERFORM
           EVALUATE WS-OPC2
               WHEN 1 PERFORM 02-01-01-COMPRA-VENTA
               WHEN 2 PERFORM 02-01-02-FORMAS-PAGO
           END-EVALUATE.

       02-01-01-COMPRA-VENTA.
           PERFORM 02-01-01-1-COMPRAS
           PERFORM 02-01-01-2-VENTAS
           DISPLAY 'OPRIMA ENTER PARA CONTINUAR' 
                                            LINE 24 POSITION 30
           ACCEPT WS-ENTER                  LINE 24 POSITION 65.

       02-01-01-1-COMPRAS.
           DISPLAY CLEAR-SCREEN
           DISPLAY 'COMPRAS'                LINE 03 POSITION 05 
                   'CANTIDAD        VALOR DIVISA     VALOR TOTAL'
                                            LINE 04 POSITION 12
                   'DOLARES  '              LINE 05 POSITION 01
                   'EUROS    '              LINE 06 POSITION 01
                   'LIBRAS   '              LINE 07 POSITION 01
                   'YENES    '              LINE 08 POSITION 01
                   'DOL. CANA'              LINE 09 POSITION 01
           MOVE 1 TO O
           MOVE 5 TO LI
           PERFORM VARYING D FROM 1 BY 1 UNTIL D > 5
               AFTER F FROM 1 BY 1 UNTIL F > 3
                   ADD TAB-CANTIDA(O D F) TO WS-ACUM-CANTI
                   ADD TAB-TOTDIVI(O D F) TO WS-ACUM-TOTDI
                   IF WS-VALI-DIVIS = ZEROS
                       MOVE TAB-VALDIVI(O D F) TO WS-VALI-DIVIS
                   END-IF
                   EVALUATE D ALSO F
                    WHEN 1 ALSO 3
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 2 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 3 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 4 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 5 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                   END-EVALUATE
           END-PERFORM.

       02-01-01-2-VENTAS.
           PERFORM 999-FECHAS
           DISPLAY 'VENTAS'                LINE 12 POSITION 05 
                   'CANTIDAD        VALOR DIVISA     VALOR TOTAL'
                                            LINE 13 POSITION 12
                   'DOLARES  '              LINE 14 POSITION 01
                   'EUROS    '              LINE 15 POSITION 01
                   'LIBRAS   '              LINE 16 POSITION 01
                   'YENES    '              LINE 17 POSITION 01
                   'DOL. CANA'              LINE 18 POSITION 01
           MOVE 2 TO O
           MOVE 14 TO LI
           PERFORM VARYING D FROM 1 BY 1 UNTIL D > 5
               AFTER F FROM 1 BY 1 UNTIL F > 3
                   ADD TAB-CANTIDA(O D F) TO WS-ACUM-CANTI
                   ADD TAB-TOTDIVI(O D F) TO WS-ACUM-TOTDI
                   IF WS-VALI-DIVIS = ZEROS
                       MOVE TAB-VALDIVI(O D F) TO WS-VALI-DIVIS
                   END-IF
                   EVALUATE D ALSO F
                    WHEN 1 ALSO 3
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 2 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 3 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 4 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                    WHEN 5 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN
                   END-EVALUATE
           END-PERFORM.

       02-01-01-1-MOSTRAR-PAN.
          MOVE WS-ACUM-CANTI TO WS-MASCA-CANTI
          DISPLAY WS-MASCA-CANTI 
                                LINE LI POSITION 16
          MOVE WS-VALI-DIVIS TO WS-MASCA-DIVI
          DISPLAY WS-MASCA-DIVI LINE LI POSITION 33
          MOVE WS-ACUM-TOTDI TO WS-MASCA-OPER
          DISPLAY WS-MASCA-OPER LINE LI POSITION 56
          MOVE ZEROS TO WS-ACUM-CANTI WS-ACUM-TOTDI WS-VALI-DIVIS. 
                           
       02-01-02-FORMAS-PAGO.
           DISPLAY CLEAR-SCREEN
           PERFORM 999-FECHAS
           DISPLAY 'FORMAS DE PAGO'         line 03 position 05
                    'COMPRAS'               LINE 04 POSITION 05
                    'CANTIDAD        VALOR TOTAL'
                                            LINE 05 POSITION 12
                    'EFECTIVO'              LINE 06 POSITION 01
                    'DEBITO  '              LINE 07 POSITION 01
                    'CREDITO '              LINE 08 POSITION 01
           MOVE 1 TO O
           MOVE 06 TO LI
           PERFORM VARYING F FROM 1 BY 1 UNTIL F > 3
               AFTER D FROM 1 BY 1 UNTIL D > 5
                   ADD TAB-CANTIDA(O D F) TO WS-ACUM-CANTI
                   ADD TAB-TOTDIVI(O D F) TO WS-ACUM-TOTDI
                   EVALUATE D ALSO F
                    WHEN 5 ALSO 1
                      PERFORM 02-01-01-1-MOSTRAR-PAN2
                    WHEN 5 ALSO 2
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN2
                    WHEN 5 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN2
                   END-EVALUATE
           END-PERFORM
           DISPLAY 'VENTAS'                 LINE 11 POSITION 05
                    'CANTIDAD        VALOR TOTAL'
                                            LINE 13 POSITION 12
                    'EFECTIVO'              LINE 14 POSITION 01
                    'CHEQUES '              LINE 15 POSITION 01
                    'TRASNFER'              LINE 16 POSITION 01
           MOVE 2 TO O
           MOVE 14 TO LI
           PERFORM VARYING F FROM 1 BY 1 UNTIL F > 3
               AFTER D FROM 1 BY 1 UNTIL D > 5
                   ADD TAB-CANTIDA(O D F) TO WS-ACUM-CANTI
                   ADD TAB-TOTDIVI(O D F) TO WS-ACUM-TOTDI
                   EVALUATE D ALSO F
                    WHEN 5 ALSO 1
                      PERFORM 02-01-01-1-MOSTRAR-PAN2
                    WHEN 5 ALSO 2
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN2
                    WHEN 5 ALSO 3
                      ADD 1 TO LI
                      PERFORM 02-01-01-1-MOSTRAR-PAN2
                   END-EVALUATE
           END-PERFORM
           DISPLAY 'OPRIMA ENTER PARA CONTINUAR' 
                                            LINE 24 POSITION 30
           ACCEPT WS-ENTER                  LINE 24 POSITION 65.
                  
       02-01-01-1-MOSTRAR-PAN2.
           MOVE WS-ACUM-CANTI TO WS-MASCA-CANTI
           DISPLAY WS-MASCA-CANTI 
                                LINE LI POSITION 16
           MOVE WS-ACUM-TOTDI TO WS-MASCA-OPER
           DISPLAY WS-MASCA-OPER LINE LI POSITION 33
           MOVE ZEROS TO WS-ACUM-CANTI WS-ACUM-TOTDI.

       3000-FINAL.
           STOP RUN.
