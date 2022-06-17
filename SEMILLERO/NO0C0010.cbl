
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
      *> PROGRAMA QUE CALCULE EL INTERES COMPUESTO Y SIMPLE
      *> P CAPITAL
      *> R-I INTERES %
      *> N PERIODOS
      *> SALIDA EN PESOS
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
           02 WS-ENTER                    PIC X(01) VALUE SPACES.

      *----------------------------------------------------------------*
      * INTERES SIMPLE EN MESES
      * TOTS: Cantidad final
      * IS-C: Capital
      * IS-I: Tasa de interés
      * T: Tiempo en meses
      *----------------------------------------------------------------*
           02 WS-SIMPLE.
              03 WS-TOTS                 PIC 9(15)V9(02).
              03 WS-IS-C                 PIC 9(15)V9(02).
              03 WS-IS-I                 PIC 9(03)V9(04).
              03 WS-IS-TI                PIC 9(15)V9(02).
              03 WS-IS-T                 PIC 9(02).
              03 WS-MAS-ST               PIC $$$$,$$$,$$$,$$$,$$$.9(02).
              03 WS-MAS-SI               PIC $$$$,$$$,$$$,$$$,$$$.9(02).

      *----------------------------------------------------------------*
      * INTERES COMPUESTO
      * TOTC: Cantidad final
      * IC-C: Capital
      * IC-I: Intereses
      * IC-T: Tiempo en meses
      *----------------------------------------------------------------*
           02 WS-COMPUESTO.
              03 WS-TOTC                 PIC 9(15)V9(02).
              03 WS-IC-C                 PIC 9(15)V9(02).
              03 WS-IC-I                 PIC 9(03)V9(02).
              03 WS-IC-T                 PIC 9(02).
              03 WS-MAS-CT               PIC $(15).9(02).
              03 WS-MAS-CI               PIC $(15).9(02).

      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
           02 WS-FECHA-ACT               PIC 9(06) VALUE ZEROES.
           02 WS-HORA-ACT                PIC 9(08) VALUE ZEROES.
           02 WS-FECHA-SIS.
              03 WS-DIA-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE '/'.
              03 WS-MES-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE '/'.
              03 WS-SIG-SIS              PIC 9(02) VALUE 20.
              03 WS-ANO-SIS              PIC 9(02) VALUE ZEROES.

           02 WS-HORA-SIS.
              03 WS-HOR-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE ':'.
              03 WS-MIN-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE ':'.
              03 WS-SEG-SIS              PIC 9(02) VALUE ZEROES.
       
       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-FECHAS
       PERFORM 2002-PANTALLA-FECHAS
       PERFORM 2003-SIMPLE
       PERFORM 2004-COMPUESTO
       PERFORM 3000-FINAL.

      *----------------------------------------------------------------*
      * OBTENER LA FECHA Y LA HORA DEL SISTEMA
      *----------------------------------------------------------------*
       2001-FECHAS.
           ACCEPT WS-FECHA-ACT           FROM DATE 
           MOVE WS-FECHA-ACT(5:2)        TO WS-DIA-SIS
           MOVE WS-FECHA-ACT(3:2)        TO WS-MES-SIS
           MOVE WS-FECHA-ACT(1:2)        TO WS-ANO-SIS
           ACCEPT WS-HORA-ACT            FROM TIME
           MOVE WS-HORA-ACT(1:2)         TO WS-HOR-SIS
           MOVE WS-HORA-ACT(3:2)         TO WS-MIN-SIS
           MOVE WS-HORA-ACT(5:2)         TO WS-SEG-SIS.

       2002-PANTALLA-FECHAS.
           DISPLAY 'FEC SIS: '           LINE 01 POSITION 01
           DISPLAY WS-FECHA-SIS          LINE 01 POSITION 10          
           DISPLAY 'HORA SIS: '          LINE 01 POSITION 62
           DISPLAY WS-HORA-SIS           LINE 01 POSITION 72.         

       2003-SIMPLE.
      * SIMPLE: pago mensual
           INITIALIZE WS-SIMPLE
           DISPLAY 'INTERES SIMPLE: '    LINE 02 POSITION 01
           DISPLAY 'CAPITAL: '           LINE 03 POSITION 01
           ACCEPT WS-IS-C                LINE 03 POSITION 18
           DISPLAY 'TASA DE INTERES: '   LINE 04 POSITION 01
           ACCEPT WS-IS-I                LINE 04 POSITION 18
           DISPLAY 'TIEMPO EN MESES: '   LINE 05 POSITION 01
           ACCEPT WS-IS-T                LINE 05 POSITION 18
           DIVIDE 100 INTO WS-IS-C END-DIVIDE
           DIVIDE 100 INTO WS-IS-I END-DIVIDE
      * Hallar el interés
           COMPUTE WS-IS-TI = (WS-IS-C * WS-IS-I / 100) * WS-IS-T
               ON SIZE ERROR
                   DISPLAY 'ERROR AL HALLAR EL INTERES SIMPLE'
                                         LINE 06 POSITION 01
               NOT ON SIZE ERROR
                 MOVE WS-IS-TI           TO WS-MAS-SI
                 DISPLAY 'INTERES A PAGAR: '
                                         LINE 06 POSITION 01
                 DISPLAY WS-MAS-SI       LINE 06 POSITION 18
           END-COMPUTE
      *> * Encontrar el valor total
           ADD WS-IS-C TO WS-IS-TI GIVING WS-TOTS 
               ON SIZE ERROR 
                   DISPLAY 'ERROR AL HALLAR EL TOTAL'
               NOT ON SIZE ERROR
                   MOVE WS-TOTS          TO WS-MAS-ST
                   DISPLAY 'TOTAL A PAGAR: ' 
                                         LINE 07 POSITION 01
                   DISPLAY WS-MAS-ST     LINE 07 POSITION 18
           END-ADD
           PERFORM 2005-SALIR.

       2004-COMPUESTO.
      * Compuesta intereses sobre intereses
           INITIALIZE WS-COMPUESTO
           DISPLAY CLEAR-SCREEN
           PERFORM 2002-PANTALLA-FECHAS
           DISPLAY 'INTERES COMPUESTO:'  LINE 02 POSITION 01
           DISPLAY 'CAPITAL: '           LINE 03 POSITION 01
           ACCEPT WS-IC-C                LINE 03 POSITION 18
           DISPLAY 'TASA DE INTERES: '   LINE 04 POSITION 01
           ACCEPT WS-IC-I                LINE 04 POSITION 18
           DISPLAY 'TIEMPO EN MESES: '   LINE 05 POSITION 01
           ACCEPT WS-IC-T                LINE 05 POSITION 18
      * Hallar el total a pagar
           DIVIDE WS-IC-I BY 100 GIVING WS-IC-I END-DIVIDE
           COMPUTE WS-TOTC = WS-IC-C * (1 + WS-IC-I)** WS-IC-T
               ON SIZE ERROR
                   DISPLAY 'ERROR AL HALLAR EL VALOR TOTAL'
                                         LINE 07 POSITION 01
               NOT ON SIZE ERROR 
                   MOVE WS-TOTC          TO WS-MAS-CT
                   DISPLAY 'TOTAL A PAGAR:'
                                         LINE 07 POSITION 01
                   DISPLAY WS-MAS-CT     LINE 07 POSITION 18
           END-COMPUTE
           SUBTRACT WS-IC-C FROM WS-TOTC 
               ON SIZE ERROR
                   DISPLAY 'ERROR AL HALLAR EL INTERES COMPUESTO'
                                         LINE 06 POSITION 01
               NOT ON SIZE ERROR
                   DISPLAY 'TOTAL INTERES:'
                                         LINE 06 POSITION 01
                   MOVE WS-TOTC          TO WS-MAS-CT
                   DISPLAY WS-MAS-CT     LINE 06 POSITION 18
           END-SUBTRACT
           PERFORM 2005-SALIR.

       2005-SALIR.
           DISPLAY '<OPRIMA ENTER>'       LINE 24 POSITION 33      
           ACCEPT WS-ENTER                LINE 24 POSITION 48.

       3000-FINAL.
           STOP RUN.
