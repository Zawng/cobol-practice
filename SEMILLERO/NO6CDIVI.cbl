      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      * OBJETIVO: RUTINA QUE DEVUELVE EL VALOR DE LA DIVISA            *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       NO6CDIVI.
       AUTHOR.                           NOVATEC SOLUTIONS (EDWIN PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     18-JUL-22.

      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CORRECTO               PIC X VALUE 'N'.
           88 SW-INCORRECTO          VALUE 'N'.
           88 SW-CORRECTO            VALUE 'S'.       

        01 VALOR-DIVISAS-TRM.
           02 VAL-USD PIC 9(5)V99    VALUE 4358.85.
           02 VAL-EUR PIC 9(5)V99    VALUE 4418.00.
           02 VAL-GBP PIC 9(5)V99    VALUE 5218.85.
           02 VAL-JPY PIC 9(5)V99    VALUE 0031.21.
           02 VAL-CAD PIC 9(5)V99    VALUE 3332.11.
       01  TABLA-VAL-DIVISAS         REDEFINES VALOR-DIVISAS-TRM.
           02 TAB-DIV-VAL            OCCURS 5 TIMES PIC 9(5)V99.
       01  WS-DIVISAS-OK             PIC X(3) VALUE SPACES.
           88 DIVI-OK                VALUES ARE 'USD' 'EUR' 'GBP' 
                                                'JPY' 'CAD'.
       01  WS-D                      PIC 9 VALUE ZEROS.

       LINKAGE SECTION.
       COPY './COPYS/NOCODIVI.CPY'.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING NOCODIVI.
       INICIO.
           PERFORM 01-VALIDA-PARAMETROS-ENTRADA
           IF SW-CORRECTO
               PERFORM 02-CALCULAR-DIVISA
           END-IF 
           EXIT PROGRAM.

       01-VALIDA-PARAMETROS-ENTRADA.
      * VALIDAR SI LA OPERACION ES VALIDA 
           IF CDIVI-E-OPERA = 'C' OR 'V'
               SET SW-CORRECTO TO TRUE
           ELSE
               MOVE '01' TO CDIVI-R-CODRETO
               SET SW-INCORRECTO TO TRUE
           END-IF
      * VALIDAR SI LA DIVISA ES VALIDA 
           MOVE CDIVI-E-DIVISA TO WS-DIVISAS-OK
           IF DIVI-OK
               SET SW-CORRECTO TO TRUE
           ELSE
               MOVE '02' TO CDIVI-R-CODRETO
               SET SW-INCORRECTO TO TRUE
           END-IF.

       02-CALCULAR-DIVISA.
           EVALUATE CDIVI-E-DIVISA
             WHEN 'USD' MOVE 1 TO WS-D
             WHEN 'EUR' MOVE 2 TO WS-D
             WHEN 'GBP' MOVE 3 TO WS-D
             WHEN 'JPY' MOVE 4 TO WS-D
             WHEN 'CAD' MOVE 5 TO WS-D
           END-EVALUATE.

           EVALUATE CDIVI-E-OPERA
             WHEN 'C'
               COMPUTE CDIVI-S-VALDIVI = TAB-DIV-VAL(WS-D) - 
                       (TAB-DIV-VAL(WS-D) * 0.02)
             WHEN 'V'
               COMPUTE CDIVI-S-VALDIVI = TAB-DIV-VAL(WS-D) + 
                       (TAB-DIV-VAL(WS-D) * 0.04)
           END-EVALUATE
           MOVE '00' TO CDIVI-R-CODRETO.
