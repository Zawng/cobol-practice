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
      * [] INTERES MENSUAL [] SEGURO MENSUAL [] TOTAL MENSUAL
      * [] TOTAL A PAGAR DURANTE EL TIEMPO
      * INTERES, CAPITAL, SEGURO, total mensual
      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       O0C0004.
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
      * PRODUCTOS
      *----------------------------------------------------------------*
       01  WS-CAPITAL                 PIC 9(15)V99 VALUE ZEROS.
      * MASCARA CAPITAL 
       01  WS-MAS-CAP                 PIC $(14)9.99.

       01  WS-PRODUCTO                PIC 9     VALUE ZEROS.
       01  WS-PRODUCTO-SELECCIONADO   PIC X(24).
           88 WS-PRE-TDC              VALUE 'TARJETA DE CREDITO'.
           88 WS-PRE-HIP              VALUE 'PRESTAMO HIPOTECARIO'.
           88 WS-PRE-VEH              VALUE 'PRESTAMO VEHICULO'.
           88 WS-PRE-INV              VALUE 'PRESTAMO LIBRE INVERSION'.
           88 WS-PRE-EDU              VALUE 'PRESTAMO EDUCACION'.

      *----------------------------------------------------------------*
      * INTERESES
      *----------------------------------------------------------------*
       01  WS-SEGURO                 PIC 99V9   VALUE 01.5.
       01  WS-INTERES                 PIC 99.9  VALUE ZEROS.
           88 WS-INT-TDC              VALUE 30.0.
           88 WS-INT-HIP              VALUE 16.0.
           88 WS-INT-VEH              VALUE 18.0.
           88 WS-INT-INV              VALUE 24.0.
           88 WS-INT-EDU              VALUE 19.0.


      *----------------------------------------------------------------*
      * MAXIMO AÑOS
      *----------------------------------------------------------------*
       01  WS-ANO                     PIC 99    VALUE ZEROS.
           88 WS-ANO-TDC              VALUE 05.
           88 WS-ANO-HIP              VALUE 20.
           88 WS-ANO-VEH              VALUE 06.
           88 WS-ANO-INV              VALUE 05.
           88 WS-ANO-EDU              VALUE 07.

      *----------------------------------------------------------------*
      * TIPO PERSONA
      *----------------------------------------------------------------*
       01  WS-GENERO                  PIC A     VALUE SPACES.
       01  WS-GENERO-SELECCIONADO     PIC A     VALUE SPACES.
           88 WS-PER-HOM              VALUE 'HOMBRE'.
           88 WS-PER-MUJ              VALUE 'MUJER'.

       01  WS-CABEZA-HOGAR            PIC A     VALUE SPACES.
       01  WS-HOGAR                   PIC A(02) VALUE SPACES.
           88 WS-HOG-SI               VALUE 'SI'.
           88 WS-HOG-NO               VALUE 'NO'.

      *----------------------------------------------------------------*
      * BANNER
      *----------------------------------------------------------------*
       01  WS-ASTERISCOS              PIC X(80) VALUE ALL '*'.
       01  WS-BANNER.
           02 FILLER                  PIC X(16) VALUE ALL '-'.
           02 FILLER                  PIC A(49) VALUE 'VALOR DE CUOTA A P
      -      'AGAR DEPENDIENDO DE UN PRODUCTO'.
           02 FILLER                  PIC X(15) VALUE ALL '-'.

      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPCION                 PIC A VALUE SPACES.

      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
       01 WS-FECHA-ACT               PIC 9(06) VALUE ZEROES.
       01 WS-HORA-ACT                PIC 9(08) VALUE ZEROES.
       01 WS-FECHA-SIS.
          02 WS-DIA-SIS              PIC 9(02) VALUE ZEROES.
          02 FILLER                  PIC X(01) VALUE '/'.
          02 WS-MES-SIS              PIC 9(02) VALUE ZEROES.
          02 FILLER                  PIC X(01) VALUE '/'.
          02 WS-SIG-SIS              PIC 9(02) VALUE 20.
          02 WS-ANO-SIS              PIC 9(02) VALUE ZEROES.

       01 WS-HORA-SIS.
          02 WS-HOR-SIS              PIC 9(02) VALUE ZEROES.
          02 FILLER                  PIC X(01) VALUE ':'.
          02 WS-MIN-SIS              PIC 9(02) VALUE ZEROES.
          02 FILLER                  PIC X(01) VALUE ':'.
          02 WS-SEG-SIS              PIC 9(02) VALUE ZEROES.
       
       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-FECHAS
       PERFORM 2002-PANTALLA-FECHAS
       PERFORM 2004-INFORMACION
       PERFORM 2007-PROCESOS
       PERFORM 2008-SALIDAS
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
           PERFORM 2003-BANNER
           DISPLAY 'INGRESE EL CAPITAL:' LINE 07 POSITION 01
           ACCEPT WS-CAPITAL             LINE 07 POSITION 21
           PERFORM 2005-PRODUCTOS.

       2005-PRODUCTOS.
           DISPLAY 'SELECCIONE UN PRODUCTO:'
                                         LINE 09 POSITION 01
           DISPLAY '1) TARJETA DE CREDITO       - 30%, 05 ANOS'
                                         LINE 10 POSITION 01
           DISPLAY '2) PRESTAMO HIPOTECARIO     - 16%, 20 ANOS'
                                         LINE 11 POSITION 01
           DISPLAY '3) PRESTAMO DE VEHICULO     - 18%, 06 ANOS'
                                         LINE 12 POSITION 01
           DISPLAY '4) PRESTAMO LIBRE INVERSION - 24%, 05 ANOS'
                                         LINE 13 POSITION 01
           DISPLAY '5) PRESTAMO PARA EDUCACION  - 19%, 07 ANOS'
                                         LINE 14 POSITION 01
           DISPLAY 'OPCION) '            LINE 15 POSITION 01
           ACCEPT WS-PRODUCTO            LINE 15 POSITION 09
           DISPLAY 'SELECCIONE SU GENERO:'
                                         LINE 17 POSITION 01
           DISPLAY 'H/h) HOMBRE:'        LINE 18 POSITION 01
           DISPLAY 'M/M) MUJER:'         LINE 19 POSITION 01
           DISPLAY 'OPCION) '            LINE 20 POSITION 01
           ACCEPT WS-GENERO              LINE 20 POSITION 09
           DISPLAY 'CABEZA DE HOGAR?: ' LINE 22 POSITION 01
           DISPLAY 'S-s) SI / N-n) NO'   LINE 23 POSITION 01
           DISPLAY 'OPCION) '            LINE 24 POSITION 01
           ACCEPT WS-CABEZA-HOGAR        LINE 24 POSITION 09
           DISPLAY CLEAR-SCREEN.

       2006-VALIDACIONES-PRODUCTOS.
          *>  IF WS-GENERO-SELECCIONADO = WS-PER-HOM
          *>     SET WS-PER-HOM TO TRUE           


           IF WS-PRODUCTO <= 0 OR > 5 THEN
               PERFORM 2009-OPCION-NO-ENCONTRADA
           ELSE
               IF WS-PRODUCTO = 1 THEN
                   SET WS-PRE-TDC TO TRUE
               END-IF
               IF WS-PRODUCTO = 2 THEN
                   SET WS-PRE-HIP TO TRUE
               END-IF
               IF WS-PRODUCTO = 3 THEN
                   SET WS-PRE-VEH TO TRUE
               END-IF
               IF WS-PRODUCTO = 4 THEN
                   SET WS-PRE-INV TO TRUE
               END-IF
               IF WS-PRODUCTO = 5 THEN
                   SET WS-PRE-EDU TO TRUE
               END-IF
           END-IF.

       2007-PROCESOS.
           PERFORM 2002-PANTALLA-FECHAS
           PERFORM 2003-BANNER
           PERFORM 2006-VALIDACIONES-PRODUCTOS.

       2008-SALIDAS.
           DISPLAY 'CAPITAL:'            LINE 07 POSITION 01 
           DISPLAY WS-MAS-CAP            LINE 07 POSITION 10
           DISPLAY 'PRODUCTO SELECCIONADO:'
                                         LINE 07 POSITION 01
           DISPLAY WS-PRODUCTO-SELECCIONADO
                                         LINE 07 POSITION 25.

       2009-OPCION-NO-ENCONTRADA.
           DISPLAY CLEAR-SCREEN
           PERFORM 2002-PANTALLA-FECHAS
           PERFORM 2003-BANNER
           DISPLAY 'OPCION NO ENCONTRADA'
                                     LINE 07 POSITION 01
           PERFORM 2020-OPCION
           PERFORM 3000-FINAL.

       2020-OPCION.
           DISPLAY 'PRESIONE UNA TECLA PARA CONTINUAR'
                                         LINE 24 POSITION 23
           ACCEPT WS-OPCION              LINE 24 POSITION 57. 
 
       3000-FINAL.
           STOP RUN.
