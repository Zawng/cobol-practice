      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * CALCULAR VALOR DE LA CUOTA QUE DEBE PAGAR UN CLIENTE DEPENDIENDO
      * DEL TIPO DE PRODUCTO QUE SELECCIONE
      * [PRODUCTOS] Interés efectivo anual
      * 1. TARJETA DE CREDITO:       30%, 05 AÑOS
      * 2. PRESTAMO HIPOTECARIO:     16%, 20 AÑOS
      * 3. PRESTAMO VEHICULO:        18%, 06 AÑOS
      * 4. PRESTAMO LIBRE INVERSION: 24%, 05 AÑOS
      * 5. PRESTAMO PARA EDUCACION:  19%, 07 AÑOS
      * SEGURO:                   1.5%

      * [VALIDACIONES] HOMBRE / MUJER
      * ¿CABEZA DE HOGAR?
      * MUJER: -2% AL PRODUCTO
      * HOMBRE: -1.5% AL PRODUCTO

      * SALIDA:
      * [] INTERES MENSUAL [X] SEGURO MENSUAL [] TOTAL MENSUAL
      * [] TOTAL A PAGAR DURANTE EL TIEMPO
      * INTERES, CAPITAL, SEGURO, total mensual
      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       NO0C0011.
       AUTHOR.                           NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     23-JUN-22.

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
      * TITULO PANTALLA
      *----------------------------------------------------------------*
       01  WS-ASTERISCOS              PIC X(80) VALUE ALL '*'.
       01  WS-BANNER.
           02 FILLER                  PIC X(16) VALUE ALL '-'.
           02 FILLER                  PIC A(49) VALUE 'VALOR DE CUOTA A 
      -      'PAGAR DEPENDIENDO DE UN PRODUCTO'.
           02 FILLER                  PIC X(15) VALUE ALL '-'.

      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
       01  WS-FECHA-ACT               PIC 9(06) VALUE ZEROS.
       01  WS-HORA-ACT                PIC 9(08) VALUE ZEROS.
       01  WS-FECHA-SIS.
           02 WS-DIA-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE '/'.
           02 WS-MES-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE '/'.
           02 WS-SIG-SIS              PIC 9(02) VALUE 20.
           02 WS-ANO-SIS              PIC 9(02) VALUE ZEROS.

       01  WS-HORA-SIS.
           02 WS-HOR-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE ':'.
           02 WS-MIN-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE ':'.
           02 WS-SEG-SIS              PIC 9(02) VALUE ZEROS.

      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPCION                 PIC A(01) VALUE SPACES.

      *----------------------------------------------------------------*
      * INTERESES
      *----------------------------------------------------------------*
       01  WS-SEGURO                  PIC 9V9(03) VALUE 0.015.
       01  WS-SEG-TOT                 PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-SEG-TOT-MAS             PIC $$$,$$$,$$$,$$$,$$9.9(02). 
       01  WS-MAS-SEG                 PIC 9.9(03).

       01  WS-MAS-INT                 PIC Z9.9(03).
       01  WS-INTERES                 PIC 9V9(03) VALUE ZEROS.
      * HOMBRE O MUJER NO CABEZA DE HOGAR 
           88 WS-INT-TDC              VALUE 0.300.
           88 WS-INT-HIP              VALUE 0.160.
           88 WS-INT-VEH              VALUE 0.180.
           88 WS-INT-INV              VALUE 0.240.
           88 WS-INT-EDU              VALUE 0.190.

      * HOMBRE CABEZA DE HOGAR
           88 WS-HOM-TDC              VALUE 0.285.
           88 WS-HOM-HIP              VALUE 0.145.
           88 WS-HOM-VEH              VALUE 0.165.
           88 WS-HOM-INV              VALUE 0.225.
           88 WS-HOM-EDU              VALUE 0.175.

      * MUJER CABEZA DE HOGAR
           88 WS-MUJ-TDC              VALUE 0.280.
           88 WS-MUJ-HIP              VALUE 0.140.
           88 WS-MUJ-VEH              VALUE 0.160.
           88 WS-MUJ-INV              VALUE 0.220.
           88 WS-MUJ-EDU              VALUE 0.170.

      *----------------------------------------------------------------*
      * PRODUCTOS
      *----------------------------------------------------------------*
       01  WS-CAPITAL                 PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-MAS-CAP                 PIC $$$,$$$,$$$,$$$,$$9.9(02).

       01  WS-ANO-TOT                 PIC 9(02) VALUE ZEROS.

       01  WS-PRODUCTO                PIC 9(01) VALUE ZEROS.
       01  WS-PRO-SEL                 PIC X(24) VALUE SPACES.
           88 WS-PRO-TDC              VALUE 'TARJETA DE CREDITO'.
           88 WS-PRO-HIP              VALUE 'PRESTAMO HIPOTECARIO'.
           88 WS-PRO-VEH              VALUE 'PRESTAMO VEHICULO'.
           88 WS-PRO-INV              VALUE 'PRESTAMO LIBRE INVERSION'.
           88 WS-PRO-EDU              VALUE 'PRESTAMO EDUCACION'.

       01  WS-GENERO                  PIC A(01) VALUE SPACES.
       01  WS-GEN-SEL                 PIC A(06) VALUE SPACES.

       01  WS-HOGAR                   PIC A(02) VALUE SPACES.

       01  WS-MES-TOT                 PIC 9(15)V9(02) VALUE ZEROES.
       01  WS-MES-TOT-MAS             PIC $$$,$$$,$$$,$$$,$$9.9(02).

       01  WS-MAS-TOT                 PIC $$$,$$$,$$$,$$$,$$9.9(02).
       01  WS-TOTAL                   PIC 9(15)V9(02) VALUE ZEROS.

       01  WS-SEGURO-MAS              PIC $$$,$$$,$$$,$$$,$$9.9(02).
       01  WS-SEGURO-TOT              PIC 9(15)V9(02) VALUE ZEROS.

       01  WS-INTERES-MAS             PIC $$$,$$$,$$$,$$$,$$9.9(02).
       01  WS-INTERES-TOT             PIC 9(15)V9(02) VALUE ZEROS.
       
       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-FECHAS
       PERFORM 2004-INFORMACION
       PERFORM 2008-HALLAR-INTERES
       PERFORM 2009-HALLAR-PRODUCTO
       PERFORM 2010-HALLAR-HOGAR
       PERFORM 2011-HALLAR-SEGURO-MENSUAL
       PERFORM 2013-HALLAR-INTERESES-MENSUALES.
       PERFORM 2014-HALLAR-TOTAL
       PERFORM 2015-HALLAR-MENSUALES-TOTALES.
       PERFORM 2019-SALIDA
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

       2003-BANNER.
           DISPLAY WS-ASTERISCOS         LINE 03 POSITION 01
           DISPLAY WS-BANNER             LINE 04 POSITION 01
           DISPLAY WS-ASTERISCOS         LINE 05 POSITION 01.

       2004-INFORMACION.
           PERFORM 2002-PANTALLA-FECHAS
           PERFORM 2003-BANNER
      * CAPITAL
           DISPLAY 'INGRESE EL CAPITAL:' LINE 07 POSITION 01
           ACCEPT WS-CAPITAL             LINE 07 POSITION 21
           DIVIDE 100 INTO WS-CAPITAL    END-DIVIDE
           MOVE WS-CAPITAL               TO WS-MAS-CAP
      * TIEMPO EN AÑOS
           DISPLAY 'TIEMPO EN ANOS:'     LINE 09 POSITION 01
           ACCEPT WS-ANO-TOT             LINE 09 POSITION 21
      * TIPO DE PRODUCTO     
           DISPLAY 'SELECCIONE UN PRODUCTO:'
                                         LINE 11 POSITION 01
           DISPLAY '1) TARJETA DE CREDITO       - 30%, 05 ANOS'
                                         LINE 12 POSITION 01
           DISPLAY '2) PRESTAMO HIPOTECARIO     - 16%, 20 ANOS'
                                         LINE 13 POSITION 01
           DISPLAY '3) PRESTAMO DE VEHICULO     - 18%, 06 ANOS'
                                         LINE 14 POSITION 01
           DISPLAY '4) PRESTAMO LIBRE INVERSION - 24%, 05 ANOS'
                                         LINE 15 POSITION 01
           DISPLAY '5) PRESTAMO PARA EDUCACION  - 19%, 07 ANOS'
                                         LINE 16 POSITION 01
           DISPLAY 'OPCION) '            LINE 17 POSITION 01
           ACCEPT WS-PRODUCTO            LINE 17 POSITION 09
      * GENERO DEL USUARIO     
           DISPLAY 'SELECCIONE SU GENERO:'
                                         LINE 19 POSITION 01
           DISPLAY 'H/h) HOMBRE - M/m) MUJER' 
                                         LINE 20 POSITION 01
           DISPLAY 'OPCION) '            LINE 21 POSITION 01
           ACCEPT WS-GENERO              LINE 21 POSITION 09.
      * CABEZA DE HOGAR
           DISPLAY 'CABEZA DE HOGAR? S-s) SI / N-n) NO: ' 
                                         LINE 23 POSITION 01
           DISPLAY 'OPCION) '            LINE 24 POSITION 01
           ACCEPT WS-HOGAR               LINE 24 POSITION 09.

       2005-INTERES-INDEPENDIENTE.
           IF WS-PRODUCTO = 1 THEN
             SET WS-INT-TDC TO TRUE
           ELSE
             IF WS-PRODUCTO = 2 THEN
               SET WS-INT-HIP TO TRUE
             ELSE
               IF WS-PRODUCTO = 3 THEN
                 SET WS-INT-VEH TO TRUE
               ELSE
                 IF WS-PRODUCTO = 4 THEN
                   SET WS-INT-INV TO TRUE
                 ELSE 
                   IF WS-PRODUCTO = 5 THEN 
                     SET WS-INT-EDU TO TRUE
                   ELSE
                     PERFORM 
                       2009-OPCION-NO-ENCONTRADA
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF.

       2006-INTERES-HOMBRE-HOGAR.
           IF WS-PRODUCTO = 1 THEN
             SET WS-HOM-TDC TO TRUE
           ELSE
             IF WS-PRODUCTO = 2 THEN
               SET WS-HOM-HIP TO TRUE
             ELSE
               IF WS-PRODUCTO = 3 THEN
                 SET WS-HOM-VEH TO TRUE
               ELSE
                IF WS-PRODUCTO = 4 THEN
                  SET WS-HOM-INV TO TRUE
                ELSE 
                  IF WS-PRODUCTO = 5 THEN 
                    SET WS-HOM-EDU TO TRUE
                  ELSE
                    PERFORM 2009-OPCION-NO-ENCONTRADA
                  END-IF
                END-IF
               END-IF
             END-IF
           END-IF.

       2007-INTERES-MUJER-HOGAR.
           IF WS-PRODUCTO = 1 THEN
             SET WS-MUJ-TDC TO TRUE
           ELSE
             IF WS-PRODUCTO = 2 THEN
               SET WS-MUJ-HIP TO TRUE
             ELSE
               IF WS-PRODUCTO = 3 THEN
                 SET WS-MUJ-VEH TO TRUE
               ELSE
                 IF WS-PRODUCTO = 4 THEN
                   SET WS-MUJ-INV TO TRUE
                 ELSE 
                   IF WS-PRODUCTO = 5 THEN 
                     SET WS-MUJ-EDU TO TRUE
                   ELSE
                     PERFORM 
                       2009-OPCION-NO-ENCONTRADA
                   END-IF
                 END-IF
               END-IF
             END-IF
            END-IF.

       2008-HALLAR-INTERES.
           IF WS-GENERO = 'H' OR 'h' THEN
               MOVE 'HOMBRE' TO WS-GEN-SEL
               IF WS-HOGAR = 'S' OR 's' THEN
                  DISPLAY 'HOMBRE CABEZA DE HOGAR' LINE 03 POSITION 01
                  PERFORM 2006-INTERES-HOMBRE-HOGAR
               ELSE
                  IF WS-HOGAR = 'N' OR 'n' THEN
                    DISPLAY 'HOMBRE INDEPENDIENTE' LINE 03 POSITION 01
                    PERFORM 2005-INTERES-INDEPENDIENTE
                  END-IF
               END-IF
           ELSE
               IF WS-GENERO = 'M' OR 'm'
                   MOVE 'MUJER' TO WS-GEN-SEL
                   IF WS-HOGAR = 'S' OR 's' THEN
                       DISPLAY 'MUJER CABEZA DE HOGAR' 
                       LINE 04 POSITION 01
                       PERFORM 2007-INTERES-MUJER-HOGAR
                   ELSE
                       IF WS-HOGAR = 'N' OR 'n' THEN
                           DISPLAY 'MUJER INDEPENDIENTE'
                           LINE 04 POSITION 01
                           PERFORM 2005-INTERES-INDEPENDIENTE
                       END-IF
                   END-IF
               ELSE
                   PERFORM 2009-OPCION-NO-ENCONTRADA
               END-IF
           END-IF
           MOVE WS-INTERES TO WS-MAS-INT.

       2009-HALLAR-PRODUCTO.
           IF WS-PRODUCTO <= 0 OR > 5 
             PERFORM 2009-OPCION-NO-ENCONTRADA
           ELSE
             IF WS-PRODUCTO = 1 THEN
               SET WS-PRO-TDC TO TRUE
             ELSE
               IF WS-PRODUCTO = 2 THEN
                  SET WS-PRO-HIP TO TRUE
               ELSE
                  IF WS-PRODUCTO = 3 THEN
                    SET WS-PRO-VEH TO TRUE
                  ELSE
                     IF WS-PRODUCTO = 4 THEN
                       SET WS-PRO-INV TO TRUE
                     ELSE
                       IF WS-PRODUCTO = 5 THEN
                         SET WS-PRO-EDU TO TRUE
                       END-IF
                     END-IF
                  END-IF
               END-IF
             END-IF
           END-IF.

       2010-HALLAR-HOGAR.
           IF WS-HOGAR = 'S' OR 's' THEN
               MOVE 'SI' TO WS-HOGAR
           ELSE
               IF WS-HOGAR = 'N' OR 'n' THEN
                   MOVE 'NO' TO WS-HOGAR
               ELSE
                   PERFORM 2009-OPCION-NO-ENCONTRADA
               END-IF
           END-IF.

       2011-HALLAR-SEGURO-MENSUAL.
           COMPUTE WS-SEG-TOT ROUNDED = WS-CAPITAL * WS-SEGURO
               ON SIZE ERROR PERFORM 2009-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR MOVE WS-SEG-TOT TO WS-SEG-TOT-MAS
           END-COMPUTE.

       2013-HALLAR-INTERESES-MENSUALES.
           COMPUTE WS-MES-TOT ROUNDED = (WS-CAPITAL * WS-INTERES *
            WS-ANO-TOT) / WS-ANO-TOT
               ON SIZE ERROR PERFORM 2009-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR MOVE WS-MES-TOT TO WS-MES-TOT-MAS
           END-COMPUTE. 

       2014-HALLAR-TOTAL.
           COMPUTE WS-TOTAL ROUNDED = WS-CAPITAL + 
               (WS-SEG-TOT * (WS-ANO-TOT * 12)) +
               (WS-MES-TOT * (WS-ANO-TOT * 12))
               ON SIZE ERROR PERFORM 2009-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR MOVE WS-TOTAL TO WS-MAS-TOT
               END-COMPUTE.

       2015-HALLAR-MENSUALES-TOTALES.
           COMPUTE WS-SEGURO-TOT ROUNDED = WS-SEG-TOT * 
           (WS-ANO-TOT * 12)
               ON SIZE ERROR PERFORM 2009-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR MOVE WS-SEGURO-TOT TO WS-SEGURO-MAS
           END-COMPUTE
           COMPUTE WS-INTERES-TOT ROUNDED = WS-MES-TOT * 
           (WS-ANO-TOT * 12)
               ON SIZE ERROR PERFORM 2009-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR MOVE WS-INTERES-TOT TO WS-INTERES-MAS
           END-COMPUTE.

       2019-SALIDA.
           DISPLAY CLEAR-SCREEN
           PERFORM 2002-PANTALLA-FECHAS
           PERFORM 2003-BANNER
           DISPLAY 'CAPITAL:'            LINE 07 POSITION 01 
           DISPLAY WS-MAS-CAP            LINE 07 POSITION 25
           DISPLAY 'TIEMPO A PAGAR EN ANOS:' 
                                         LINE 08 POSITION 01
           DISPLAY WS-ANO-TOT            LINE 08 POSITION 25
           DISPLAY 'PRODUCTO SELECCIONADO:'
                                         LINE 09 POSITION 01
           DISPLAY WS-PRO-SEL            LINE 09 POSITION 25
           DISPLAY 'GENERO:'             LINE 10 POSITION 01
           DISPLAY WS-GEN-SEL            LINE 10 POSITION 25
           DISPLAY 'CABEZA DE HOGAR:'    LINE 11 POSITION 01
           DISPLAY WS-HOGAR              LINE 11 POSITION 25
           DISPLAY 'PORCENTAJE INTERES:' LINE 12 POSITION 01
           DISPLAY '%'                   LINE 12 POSITION 31
           DISPLAY WS-MAS-INT            LINE 12 POSITION 25
           DISPLAY 'SEGURO:'             LINE 13 POSITION 01
           MOVE WS-SEGURO                TO WS-MAS-SEG
           DISPLAY WS-MAS-SEG            LINE 13 POSITION 25
           DISPLAY '%'                   LINE 13 POSITION 30
           DISPLAY 'RESULTADOS:'         LINE 15 POSITION 01
           DISPLAY 'SEGURO MENSUAL:'     LINE 16 POSITION 01
           DISPLAY WS-SEG-TOT-MAS        LINE 16 POSITION 25
           DISPLAY 'INTERES MENSUAL:'    LINE 17 POSITION 01
           DISPLAY WS-MES-TOT-MAS        LINE 17 POSITION 25
           DISPLAY 'SEGURO TOTAL:'       LINE 19 POSITION 01
           DISPLAY WS-SEGURO-MAS         LINE 19 POSITION 25
           DISPLAY 'INTERES TOTAL:'      LINE 20 POSITION 01
           DISPLAY WS-INTERES-MAS        LINE 20 POSITION 25
           DISPLAY 'TOTAL A PAGAR:'      LINE 21 POSITION 01
           DISPLAY WS-MAS-TOT            LINE 21 POSITION 30.

       2009-OPCION-NO-ENCONTRADA.
           DISPLAY CLEAR-SCREEN
           PERFORM 2002-PANTALLA-FECHAS
           PERFORM 2003-BANNER
           DISPLAY 'OPCION NO ENCONTRADA'
                                         LINE 12 POSITION 30
           PERFORM 2020-OPCION
           PERFORM 3000-FINAL.

       2020-OPCION.
           DISPLAY 'PRESIONE UNA TECLA PARA CONTINUAR'
                                         LINE 24 POSITION 23
           ACCEPT WS-OPCION              LINE 24 POSITION 57. 

       3000-FINAL.
           STOP RUN.
