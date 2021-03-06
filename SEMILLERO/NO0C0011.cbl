      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * CALCULAR EL VALOR DE LA CUOTA QUE DEBE PAGAR UN CLIENTE        *
      * DEPENDIENDO DEL TIPO DE PRODUCTO QUE SELECCIONE                *
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
      * MASCARAS
      *----------------------------------------------------------------*
      * RECORDAR: MASCARAS QUE SE USAN NORMALMENTE, MULTIPLCIAR POR 100
      * PARA MOSTRARLE AL USUARIO EL PORCENTAJE CORRECTO
       01  WS-MAS-DINERO              PIC $$$,$$$,$$$,$$$,$$9.9(02). 
       01  WS-MAS-INTERES             PIC Z9.99.

      * ANOS 
       01  WS-ANO-MAS                 PIC ZZ.

      * SEGURO 
       01  WS-MAS-SEG                 PIC 9.9.

      *----------------------------------------------------------------*
      * TITULO PANTALLA
      *----------------------------------------------------------------*
       01  WS-ASTERISCOS              PIC X(80) VALUE ALL '*'.
       01  WS-BANNER.
           02 FILLER                  PIC X(16) VALUE ALL '-'.
           02 FILLER                  PIC A(49) VALUE 'VALOR DE CUOTA A 
      -      'PAGAR DEPENDIENDO DE UN PRODUCTO'.
           02 FILLER                  PIC X(15) VALUE ALL '-'.
       01  WS-MENSAJE-ERROR           PIC X(255) VALUE SPACES.

      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
       COPY './RUTINAS/VARFECHAS.CPY'.

      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPCION                 PIC A(01) VALUE SPACES.

      *----------------------------------------------------------------*
      * INTERESES
      *----------------------------------------------------------------*
       01  WS-SEGURO                  PIC 9V9(03) VALUE 0.015.
       01  WS-SEG-TOT                 PIC 9(15)V9(02) VALUE ZEROS.

       01  WS-SEGURO-PAN              PIC 99V99 VALUE ZEROS.

       01  WS-DESCUENTO               PIC 9V9(03) VALUE ZEROS.
           88 WS-DST-HOM              VALUE 0.015.
           88 WS-DST-MUJ              VALUE 0.020.
           88 WS-DST-NO               VALUE 0.000.

       01  WS-INTERES-PAN             PIC 99V99 VALUE ZEROS.
      * ASI VAN LOS PORCENTAJES DE INTERESES 
       01  WS-INTERES                 PIC 9V9(03) VALUE ZEROS.
           88 WS-INT-TDC              VALUE 0.300.
           88 WS-INT-HIP              VALUE 0.160.
           88 WS-INT-VEH              VALUE 0.180.
           88 WS-INT-INV              VALUE 0.240.
           88 WS-INT-EDU              VALUE 0.190.

      *----------------------------------------------------------------*
      * PRODUCTOS
      *----------------------------------------------------------------*
       01  WS-PRODUCTO                PIC 9(01) VALUE ZEROS.
       01  WS-PRO-SEL                 PIC X(24) VALUE SPACES.
           88 WS-PRO-TDC              VALUE 'TARJETA DE CREDITO'.
           88 WS-PRO-HIP              VALUE 'PRESTAMO HIPOTECARIO'.
           88 WS-PRO-VEH              VALUE 'PRESTAMO VEHICULO'.
           88 WS-PRO-INV              VALUE 'PRESTAMO LIBRE INVERSION'.
           88 WS-PRO-EDU              VALUE 'PRESTAMO EDUCACION'.
       01  WS-CAPITAL                 PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-CAP-MES                 PIC 9(15)V9(02) VALUE ZEROS.                 
       01  WS-ANO-TOT                 PIC 9(02) VALUE ZEROS.
       01  WS-CUOTAS                  PIC 9(03) VALUE ZEROS.
       01  WS-GENERO                  PIC A(01) VALUE SPACES.
       01  WS-GEN-SEL                 PIC A(23) VALUE SPACES
                                      JUSTIFIED RIGHT.
       01  WS-HOGAR                   PIC A(02) VALUE SPACES.
       01  WS-MES-TOT                 PIC 9(15)V9(02) VALUE ZEROES.
       01  WS-TOTAL                   PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-SEGURO-TOT              PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-INTERES-TOT             PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-CUOTA-MEN               PIC 9(15)V9(02) VALUE ZEROS.
       01  WS-VALIDADOR               PIC 9 VALUE ZEROS.
       
       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
           PERFORM 2001-FECHAS
           PERFORM 2004-INFORMACION
           PERFORM 2007-HALLAR-INTERESES
           PERFORM 2010-HALLAR-SEGURO-MENSUAL
           PERFORM 2011-HALLAR-INTERESES-MENSUALES
           PERFORM 2013-HALLAR-MENSUALES-TOTALES
           PERFORM 2014-HALLAR-CAPITAL-MENSUAL
           PERFORM 2015-HALLAR-CUOTA-MENSUAL
           PERFORM 2012-HALLAR-TOTAL
           PERFORM 2016-SALIDA
           PERFORM 3000-FINAL.

      *----------------------------------------------------------------*
      * OBTENER LA FECHA Y LA HORA DEL SISTEMA
      *----------------------------------------------------------------*
       COPY './RUTINAS/PARFECHAS.CPY'.

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
           PERFORM 2005-CAPTURA-CAMPOS.

       2005-CAPTURA-CAMPOS.
           PERFORM 2005-01-CAPTURA-PRODUCTO UNTIL WS-VALIDADOR = 1
           PERFORM 2005-02-CAPTURA-CAPITAL
           MOVE 0 TO WS-VALIDADOR
           PERFORM 2005-03-CAPTURA-TIEMPO UNTIL WS-VALIDADOR = 1
           MOVE 0 TO WS-VALIDADOR
           PERFORM 2005-04-CAPTURA-SEXO UNTIL WS-VALIDADOR = 1
           MOVE 0 TO WS-VALIDADOR
           PERFORM 2005-05-CAPTURA-HOGAR UNTIL WS-VALIDADOR = 1.

       2005-01-CAPTURA-PRODUCTO.
           DISPLAY 'SELECCIONE UN PRODUCTO:'
                                         LINE 07 POSITION 01
           DISPLAY '1) TARJETA DE CREDITO       - 30%, 05 ANOS'
                                         LINE 08 POSITION 01
           DISPLAY '2) PRESTAMO HIPOTECARIO     - 16%, 20 ANOS'
                                         LINE 09 POSITION 01
           DISPLAY '3) PRESTAMO DE VEHICULO     - 18%, 06 ANOS'
                                         LINE 10 POSITION 01
           DISPLAY '4) PRESTAMO LIBRE INVERSION - 24%, 05 ANOS'
                                         LINE 11 POSITION 01
           DISPLAY '5) PRESTAMO PARA EDUCACION  - 19%, 07 ANOS'
                                         LINE 12 POSITION 01
           DISPLAY 'OPCION) '            LINE 13 POSITION 01
           ACCEPT WS-PRODUCTO            LINE 13 POSITION 09
           PERFORM 2006-ACTIVAR-TASA-PRODUCTO.

       2005-02-CAPTURA-CAPITAL.
           DISPLAY 'INGRESE EL CAPITAL:' LINE 15 POSITION 01
           ACCEPT WS-CAPITAL             LINE 15 POSITION 21
           DIVIDE 100 INTO WS-CAPITAL    END-DIVIDE.

       2005-03-CAPTURA-TIEMPO.
           DISPLAY 'TIEMPO EN ANOS:'     LINE 17 POSITION 01
           ACCEPT WS-ANO-TOT             LINE 17 POSITION 21
           PERFORM 2006-EVALUAR-ANOS.

       2005-04-CAPTURA-SEXO.
           DISPLAY 'SELECCIONE SU GENERO:'
                                         LINE 19 POSITION 01
           DISPLAY 'H/h) HOMBRE - M/m) MUJER' 
                                         LINE 20 POSITION 01
           DISPLAY 'OPCION) '            LINE 21 POSITION 01
           ACCEPT WS-GENERO              LINE 21 POSITION 09
           PERFORM 2009-HALLAR-SEXO.

       2005-05-CAPTURA-HOGAR.
           DISPLAY 'CABEZA DE HOGAR? S-s) SI / N-n) NO: ' 
                                         LINE 23 POSITION 01
           DISPLAY 'OPCION) '            LINE 24 POSITION 01
           ACCEPT WS-HOGAR               LINE 24 POSITION 09
           PERFORM 2008-HALLAR-DESCUENTOS.

       2006-EVALUAR-ANOS.
           EVALUATE  WS-PRODUCTO
               WHEN 1
                 IF WS-ANO-TOT <= 0 OR > 5 THEN
                   MOVE 'EN LOS ANOS, INTENTE DE NUEVO' 
                   TO WS-MENSAJE-ERROR
                   PERFORM 2017-OPCION-NO-ENCONTRADA
                 ELSE
                   MOVE 1 TO WS-VALIDADOR
                 END-IF
               WHEN 2
                 IF WS-ANO-TOT <= 0 OR > 20 THEN
                   MOVE 'EN LOS ANOS, INTENTE DE NUEVO'
                   TO WS-MENSAJE-ERROR
                   PERFORM 2017-OPCION-NO-ENCONTRADA
                 ELSE
                   MOVE 1 TO WS-VALIDADOR
                 END-IF
               WHEN 3
                 IF WS-ANO-TOT <= 0 OR > 6 THEN
                   MOVE 'EN LOS ANOS, INTENTE DE NUEVO'
                   TO WS-MENSAJE-ERROR
                   PERFORM 2017-OPCION-NO-ENCONTRADA
                 ELSE
                   MOVE 1 TO WS-VALIDADOR
                 END-IF
               WHEN 4
                 IF WS-ANO-TOT <= 0 OR > 5 THEN
                   MOVE 'EN LOS ANOS, INTENTE DE NUEVO'
                   TO WS-MENSAJE-ERROR
                   PERFORM 2017-OPCION-NO-ENCONTRADA
                 ELSE
                   MOVE 1 TO WS-VALIDADOR
                 END-IF
                WHEN 5
                 IF WS-ANO-TOT <= 0 OR > 7 THEN
                   MOVE 'EN LOS ANOS, INTENTE DE NUEVO'
                   TO WS-MENSAJE-ERROR
                   PERFORM 2017-OPCION-NO-ENCONTRADA
                 ELSE
                   MOVE 1 TO WS-VALIDADOR
                 END-IF
           END-EVALUATE.

       2006-ACTIVAR-TASA-PRODUCTO.
           EVALUATE WS-PRODUCTO
               WHEN 1
                 SET WS-INT-TDC TO TRUE
                 SET WS-PRO-TDC TO TRUE
                 MOVE 1 TO WS-VALIDADOR
               WHEN 2
                 SET WS-INT-HIP TO TRUE
                 SET WS-PRO-HIP TO TRUE
                 MOVE 1 TO WS-VALIDADOR
               WHEN 3
                 SET WS-INT-VEH TO TRUE
                 SET WS-PRO-VEH TO TRUE
                 MOVE 1 TO WS-VALIDADOR
               WHEN 4
                 SET WS-INT-INV TO TRUE
                 SET WS-PRO-INV TO TRUE
                 MOVE 1 TO WS-VALIDADOR
               WHEN 5
                 SET WS-INT-EDU TO TRUE
                 SET WS-PRO-EDU TO TRUE
                 MOVE 1 TO WS-VALIDADOR
               WHEN OTHER
                 MOVE 'EN PRODUCTO, INTENTE DE NUEVO'
                 TO WS-MENSAJE-ERROR
                 PERFORM 2017-OPCION-NO-ENCONTRADA
           END-EVALUATE.

       2007-HALLAR-INTERESES.
          *>  INTERESES= TASA DE INTERES - DESCUENTOS
           SUBTRACT WS-DESCUENTO FROM WS-INTERES ROUNDED
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR
               MULTIPLY 100 BY WS-INTERES GIVING WS-INTERES-PAN ROUNDED
               MOVE WS-INTERES-PAN TO WS-MAS-INTERES
           END-SUBTRACT.

       2008-HALLAR-DESCUENTOS.
           EVALUATE WS-HOGAR
             WHEN 'S'
             WHEN 's'
              MOVE 'SI' TO WS-HOGAR
              MOVE 1 TO WS-VALIDADOR
              IF WS-GENERO = 'H' OR 'h' THEN
                SET WS-DST-HOM TO TRUE
              ELSE
                SET WS-DST-MUJ TO TRUE
              END-IF
             WHEN 'N'
             WHEN 'n'
               MOVE 1 TO WS-VALIDADOR
               MOVE 'NO' TO WS-HOGAR
                SET WS-DST-NO TO TRUE
             WHEN OTHER
               MOVE 'EN HOGAR, INTENTE DE NUEVO'
               TO WS-MENSAJE-ERROR
               PERFORM 2017-OPCION-NO-ENCONTRADA
           END-EVALUATE.

       2009-HALLAR-SEXO.
           EVALUATE WS-GENERO
             WHEN 'H'
             WHEN 'h'
               MOVE 'HOMBRE' TO WS-GEN-SEL
               MOVE 1 TO WS-VALIDADOR
             WHEN 'M'
             WHEN 'm'
               MOVE 'MUJER' TO WS-GEN-SEL
               MOVE 1 TO WS-VALIDADOR
             WHEN OTHER
               MOVE 'EN SEXO, INTENTE DE NUEVO'
               TO WS-MENSAJE-ERROR
               PERFORM 2017-OPCION-NO-ENCONTRADA
           END-EVALUATE.

       2010-HALLAR-SEGURO-MENSUAL.
           COMPUTE WS-SEG-TOT ROUNDED = WS-CAPITAL * ( WS-SEGURO / 12 )
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
           END-COMPUTE.

       2011-HALLAR-INTERESES-MENSUALES.
           COMPUTE WS-MES-TOT ROUNDED = WS-CAPITAL * (WS-INTERES
               / 12) * (WS-ANO-TOT / 12)
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
           END-COMPUTE. 

       2012-HALLAR-TOTAL.
           MULTIPLY WS-CUOTAS BY WS-CUOTA-MEN GIVING WS-TOTAL ROUNDED
           END-MULTIPLY.

       2013-HALLAR-MENSUALES-TOTALES.
           COMPUTE WS-SEGURO-TOT ROUNDED = WS-SEG-TOT * 
           (WS-ANO-TOT * 12)
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
           END-COMPUTE
           COMPUTE WS-INTERES-TOT ROUNDED = WS-MES-TOT * 
           (WS-ANO-TOT * 12)
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
           END-COMPUTE.

       2014-HALLAR-CAPITAL-MENSUAL.
      *    CAPITAL / MESES
           MULTIPLY WS-ANO-TOT BY 12 GIVING WS-CUOTAS ROUNDED 
           END-MULTIPLY
           DIVIDE WS-CUOTAS INTO WS-CAPITAL GIVING WS-CAP-MES ROUNDED
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
           END-DIVIDE.

       2015-HALLAR-CUOTA-MENSUAL.
      *     SEGURO + INTERES + CAPITAL
           ADD WS-CAP-MES WS-MES-TOT WS-SEG-TOT GIVING WS-CUOTA-MEN
               ROUNDED
               ON SIZE ERROR PERFORM 2017-OPCION-NO-ENCONTRADA
               NOT ON SIZE ERROR MOVE WS-CUOTA-MEN TO WS-MAS-DINERO
           END-ADD.

       2016-SALIDA.
           DISPLAY CLEAR-SCREEN
           PERFORM 2002-PANTALLA-FECHAS
           PERFORM 2003-BANNER
           PERFORM 2016-01-SALIDA-CAPITAL
           PERFORM 2016-02-SALIDA-ANOS
           PERFORM 2016-03-SALIDA-PRODUCTO
           PERFORM 2016-04-SALIDA-GENERO
           PERFORM 2016-05-SALIDA-HOGAR
           PERFORM 2016-06-SALIDA-PORCENTAJE-INT
           PERFORM 2016-07-SALIDA-SEGURO
           PERFORM 2016-08-RESULTADO-SEGURO-MEN
           PERFORM 2016-09-RESULTADO-INTERES-MEN
           PERFORM 2016-10-RESULTADO-CAPITAL-MEN
           PERFORM 2016-11-RESULTADO-TOTAL-MEN
           PERFORM 2016-12-RESULTADO-SEGURO-TOT
           PERFORM 2016-13-RESULTADO-INTERES-TOT
           PERFORM 2016-14-TOTAL-PAGAR.

       2016-01-SALIDA-CAPITAL.
           DISPLAY 'CAPITAL:'            LINE 07 POSITION 01 
           MOVE WS-CAPITAL               TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 07 POSITION 25.

       2016-02-SALIDA-ANOS.
           DISPLAY 'TIEMPO A PAGAR EN ANOS:' 
                                         LINE 08 POSITION 01
           MOVE WS-ANO-TOT TO WS-ANO-MAS
           DISPLAY WS-ANO-MAS            LINE 08 POSITION 25.

       2016-03-SALIDA-PRODUCTO.
           DISPLAY 'PRODUCTO SELECCIONADO:'
                                         LINE 09 POSITION 01
           DISPLAY WS-PRO-SEL            LINE 09 POSITION 25.

       2016-04-SALIDA-GENERO.
           DISPLAY 'GENERO:'             LINE 10 POSITION 01
           DISPLAY WS-GEN-SEL            LINE 10 POSITION 25.

       2016-05-SALIDA-HOGAR.
           DISPLAY 'CABEZA DE HOGAR:'    LINE 11 POSITION 01
           DISPLAY WS-HOGAR              LINE 11 POSITION 25.

       2016-06-SALIDA-PORCENTAJE-INT.
           DISPLAY 'PORCENTAJE INTERES:' LINE 12 POSITION 01
           DISPLAY '%'                   LINE 12 POSITION 30
           DISPLAY WS-MAS-INTERES        LINE 12 POSITION 25.

       2016-07-SALIDA-SEGURO.
           DISPLAY 'SEGURO:'             LINE 13 POSITION 01
           MULTIPLY 100 BY WS-SEGURO GIVING WS-SEGURO-PAN END-MULTIPLY
           MOVE WS-SEGURO-PAN            TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 13 POSITION 25
           DISPLAY '%'                   LINE 13 POSITION 47.

       2016-08-RESULTADO-SEGURO-MEN.
           DISPLAY 'RESULTADOS:'         LINE 15 POSITION 01
           DISPLAY 'SEGURO MENSUAL:'     LINE 16 POSITION 01
           MOVE WS-SEG-TOT               TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 16 POSITION 25.

      * INTERES MENSUAL
       2016-09-RESULTADO-INTERES-MEN.
           DISPLAY 'INTERES MENSUAL:'    LINE 17 POSITION 01
           MOVE WS-MES-TOT               TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 17 POSITION 25.

      * CAPITAL MENSUAL     
       2016-10-RESULTADO-CAPITAL-MEN.
           DISPLAY 'CAPITAL MENSUAL:'    LINE 18 POSITION 01
           MOVE  WS-CAP-MES              TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 18 POSITION 25.

      * VALOR TOTAL MENSUAL     
       2016-11-RESULTADO-TOTAL-MEN.
           DISPLAY 'VALOR MENSUAL: '     LINE 19 POSITION 01
           MOVE WS-CUOTA-MEN             TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 19 POSITION 25.
           
      * VALOR SEGURO TOTAL
       2016-12-RESULTADO-SEGURO-TOT.
           DISPLAY 'SEGURO TOTAL:'       LINE 21 POSITION 01
           MOVE  WS-SEGURO-TOT           TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 21 POSITION 25.

       2016-13-RESULTADO-INTERES-TOT. 
           DISPLAY 'INTERES TOTAL:'      LINE 22 POSITION 01
           MOVE WS-INTERES-TOT           TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 22 POSITION 25.

      * TOTAL A PAGAR
       2016-14-TOTAL-PAGAR.
           DISPLAY 'TOTAL A PAGAR:'      LINE 23 POSITION 01
           MOVE WS-TOTAL                 TO WS-MAS-DINERO
           DISPLAY WS-MAS-DINERO         LINE 23 POSITION 25.

       2017-OPCION-NO-ENCONTRADA.
           DISPLAY 'ERROR: '
                                         LINE 12 POSITION 50
           DISPLAY WS-MENSAJE-ERROR      LINE 12 POSITION 57.

       2018-OPCION.
           DISPLAY 'PRESIONE UNA TECLA: '
                                         LINE 14 POSITION 50
           ACCEPT WS-OPCION              LINE 14 POSITION 70. 

       3000-FINAL.
           STOP RUN.
