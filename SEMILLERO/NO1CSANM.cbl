      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      * OBJETIVO: PROGRAMA QUE REGISTRA MENSUALMENTE TODOS LOS SERVICIOS
      * QUE PRESTA LA PARROQUIA

      * 1. BAUTIZOS            50k
      * 2. PRIMERA COMUNION    80k
      * 3. CONFIRMACION        20k
      * 4. MATRIMONIO          250k 31JUL 3 NOM TEL 20%
      * 5. FUNERALES           300k
      * 6. MISAS               60k

      * HACER UN PROGRAMA QUE LE PERMITE LLEVAR LA INFORMACION DE TODOS
      * LOS SERVICIOS QUE PRESTA LA IGLESIA.
      * 1. INFORMACION QUEDE EN ARCHIVO
      * 2. REGISTRAR CADA UNO DE LOS SERVICIOS QUE LLEVO DURANTE EL MES
      * 3. CONSULTAR TODOS LOS SERVICIOS, EN CUALQUIER MOMENTO 
      * (PENDIENTES, REALIZADOS, CANCELADOS)
      * 4. MODIFICAR CUALQUIER SERVICIO / BORRARLO
      * 5. IMPRIMA TODOS LOS SERVICIOS DEL MES
      * 6. PERMITA CONSULTAR POR SERVICIO
      * lOS SERVICIOS DEBEN SER MINIMO CON UN DIA DE ANTICIPACION

      * MENU PRINCIPAL.
      *    1. CREAR ARCHIVO 
      *    2. ADICIONAR UN SERVICIO
      *    3. MODIFICAR UN SERVICIO
      *    4. BORRAR UN SERVICIO
      *    5. CONSULTAS
      *    6. IMPRIMIR
      *    7. SALIR
      *    QUE OPCION DESEA?: X
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                 NO1CSANM.
       AUTHOR.                     NOVATEC SOLUTIONS (EDWIN-PAEZ).
       INSTALLATION.               PARROQUIA SAN MIGUEL.
       DATE-WRITTEN.               22-JUL-22. 

      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * ARCHIVO QUE ALMACENA LOS SERVICIOS DE LA PARROQUIA MENSUALMENTE
           SELECT SERVICIO ASSIGN TO './FILES/SANMSERV'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS VAR-ESTADO.
      * IMPRESION DEL ARCHIVO
           SELECT IMPRESOR ASSIGN TO './FILES/IMPRESORA'
           FILE STATUS IS VAR-ESTADO.
      * CONSECUTIVO DE LOS SERVICIOS DE LA PARROQUIA
           SELECT CONSECUT ASSIGN TO './FILES/CONSECUT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS VAR-ESTADO.

      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  SERVICIO LABEL RECORD STANDARD
           RECORDING MODE IS FIXED
           BLOCK CONTAINS 0 RECORDS.
       01  REG-SERVICIO.
           02 ASER-NUM-SERVICIO    PIC 9(04).
           02 ASER-COD-SERVICIO    PIC 9(01).
           02 ASER-FECHA           PIC X(10).
           02 ASER-HORA            PIC X(05).
           02 ASER-NOMBRE          PIC X(20).
           02 ASER-TELEFONO        PIC X(10).
           02 ASER-AGREGADOS       PIC A(01).
           02 ASER-VALOR           PIC 9(07).
           02 ASER-ESTADO          PIC A(01).

       FD  IMPRESOR LABEL RECORD OMITTED
           RECORDING MODE IS FIXED
           BLOCK CONTAINS 0 RECORDS.
       01  REG-IMPRESOR        PIC X(80).

       FD  CONSECUT LABEL RECORD STANDARD
           RECORDING MODE IS FIXED
           BLOCK CONTAINS 0 RECORDS.
       01  REG-CONSECUT.
           02 ACON-CONSECUTIVO     PIC 9(04).
           

       WORKING-STORAGE SECTION.
       01  WS-OPC                  PIC 9(01) VALUE ZEROS.
       01  WS-SER                  PIC 9(01) VALUE ZEROS.
       01  WS-SEGURO               PIC A(01) VALUE SPACES.
           88 SI-SEGURO            VALUE 's' 'S'.
           88 NO-SEGURO            VALUE 'n' 'N'.
       01  WS-ENTER                PIC A(01) VALUE SPACES.
       01  WS-VALORES              PIC 9(07) VALUE ZEROS.
           88 VAL-BAU              VALUE 0050000.
           88 VAL-PRI              VALUE 0080000.
           88 VAL-CON              VALUE 0020000.
           88 VAL-MAT              VALUE 0250000.
           88 VAL-FUN              VALUE 0300000.
           88 VAL-MIS              VALUE 0060000.
       01  WS-PORCE                PIC 9(03) VALUE 020.
       01  WS-CALCULO              PIC 9(07) VALUE ZEROS.
       01  WS-ADICION              PIC A(01) VALUE SPACES.
           88 SI-ADICION           VALUE 's' 'S'.
           88 NO-ADICION           VALUE 'n' 'N'.
       01  VAR-ESTADO              PIC A(02) VALUE SPACES.

      * PROCESO
       01  WS-FEC-SIS              PIC 9(06) VALUE ZEROS.
       01  WS-HOR-SIS              PIC 9(08) VALUE ZEROS.
       01  WS-FEC-EDI.
           02 WS-FEC-EDI-DIA       PIC 9(02) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE '/'.
           02 WS-FEC-EDI-MES       PIC 9(02) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE '/'.
           02 FILLER               PIC 9(02) VALUE 20.
           02 WS-FEC-EDI-ANO       PIC 9(02) VALUE ZEROS.

       01  WS-HOR-EDI.
           02 WS-HOR-EDI-HOR       PIC 9(02) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE ':'.
           02 WS-HOR-EDI-MIN       PIC 9(02) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE ':'.
           02 WS-HOR-EDI-SEG       PIC 9(02) VALUE ZEROS.

      * SALIDA
       01  REG-SAL-ENC-01.
           02 FILLER               PIC X(09) VALUE 'FEC.SIS: '.
           02 RSAL-01-FEC-SIS      PIC X(10) VALUE SPACES.
           02 FILLER               PIC X(10) VALUE 'HORA.SIS: '.
           02 RSAL-01-HOR-SIS      PIC X(08) VALUE SPACES.

       01  REG-SAL-ENC-02.
           02 FILLER               PIC X(80) VALUE ALL '*'.

       01  REG-SAL-ENC-03.
           02 FILLER               PIC X(30) VALUE SPACES.
           02 FILLER               PIC X(20) VALUE 'PARROQUIA SAN MIGUEL
      -                                            ''.
           02 FILLER               PIC X(30) VALUE SPACES.

      * DETALLES DE PANTALLA # 1
       01  REG-SAL-DET-01.
           02 FILLER               PIC X(04) VALUE 'NUME'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(03) VALUE 'SER'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(08) VALUE 'TELEFONO'.
           02 FILLER               PIC X(09) VALUE SPACES.
           02 FILLER               PIC X(06) VALUE 'NOMBRE'.
           02 FILLER               PIC X(09) VALUE SPACES.
           02 FILLER               PIC X(05) VALUE 'FECHA'.
           02 FILLER               PIC X(03) VALUE SPACES.
           02 FILLER               PIC X(04) VALUE 'HORA'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE 'E'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(05) VALUE 'VALOR'.

      * DETALLES DE PANTALLA # 1
       01  REG-SAL-DET-02.
           02 RSAL-D02-NUM-SER     PIC 9(04) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-COD-SER     PIC X(01) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-NOM-SER     PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-TEL-CLI     PIC 9(10) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-NOM-CLI     PIC X(20) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-FEC-SER     PIC X(10) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-HOR-SER     PIC X(05) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-EST-SER     PIC X(01) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-VAL-SER     PIC $$$$,$$9 VALUE ZEROS.

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1-MENU-PRINCIPAL UNTIL WS-OPC = 7
           STOP RUN.

       1-MENU-PRINCIPAL.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'MENU PRINCIPAL       ' LINE 06 POSITION 33
                   '1. CREAR ARCHIVO     ' LINE 08 POSITION 10
                   '2. ADICIONAR SERVICIO' LINE 09 POSITION 10
                   '3. MODIFICAR SERVICIO' LINE 10 POSITION 10
                   '4. BORRAR UN SERVICIO' LINE 11 POSITION 10
                   '5. CONSULTAS         ' LINE 12 POSITION 10
                   '6. IMPRIMIR          ' LINE 13 POSITION 10
                   '7. SALIR             ' LINE 14 POSITION 10
                   'OPCION ) '             LINE 16 POSITION 20
           MOVE ZEROS TO WS-OPC
           PERFORM UNTIL WS-OPC > 0 AND < 8
               ACCEPT WS-OPC               LINE 16 POSITION 30
           END-PERFORM
           EVALUATE WS-OPC
               WHEN 1 PERFORM 1-1-CREA-ARCHIVO
               WHEN 2 PERFORM 1-2-ADICIONA-SERVICIO
      *        WHEN 3 PERFORM 1-3-MODIFICA-SERVICIO
      *        WHEN 4 PERFORM 1-4-BORRA-SERVICIO
      *        WHEN 5 PERFORM 1-5-MENU-CONSULTAS
      *        WHEN 6 PERFORM 1-6-IMPRIME-ARCHIVO
           END-EVALUATE.

       1-1-CREA-ARCHIVO.
           DISPLAY CLEAR-SCREEN
           DISPLAY 'QUIERES CREAR UN NUEVO ARCHIVO? S/N'
                                   LINE 10 POSITION 01
           MOVE SPACE TO WS-SEGURO
           PERFORM UNTIL SI-SEGURO OR NO-SEGURO
               ACCEPT WS-SEGURO    LINE 10 POSITION 50
           END-PERFORM
           IF SI-SEGURO
               OPEN OUTPUT SERVICIO CONSECUT
               CLOSE SERVICIO CONSECUT
               DISPLAY 'ARCHIVOS CREADO CON EXITO, OPRIMA ENTER'
                                   LINE 12 POSITION 01
               ACCEPT WS-ENTER     LINE 12 POSITION 50
           END-IF.

       1-2-ADICIONA-SERVICIO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'ADICION DE SERVICIO' LINE 06 POSITION 31
           DISPLAY 'SERVICIOS          ' LINE 07 POSITION 36
           DISPLAY '1. BAUTISMO        ' LINE 08 POSITION 01
           DISPLAY '4. MATRIMONIO      ' LINE 08 POSITION 53
           DISPLAY '2. PRIMERA COMUNION' LINE 09 POSITION 01
           DISPLAY '5. FUNERALES       ' LINE 09 POSITION 53
           DISPLAY '3. CONFIRMACION    ' LINE 10 POSITION 01
           DISPLAY '6. MISAS           ' LINE 10 POSITION 53
           display 'QUE SERVICIO DESEA?' LINE 11 POSITION 31
           MOVE ZEROS TO WS-SER
           PERFORM UNTIL WS-SER > 0 AND < 7
               ACCEPT WS-SER             LINE 11 POSITION 51
           END-PERFORM.

       999-ENCABEZADO-PAN.
           DISPLAY CLEAR-SCREEN
           DISPLAY REG-SAL-ENC-01
      * TOMAR FECHA DEL SISTEMA Y MODIFICAR EL FORMATO SOLICITADO
           ACCEPT WS-FEC-SIS       FROM DATE
           MOVE WS-FEC-SIS(5:2)    TO WS-FEC-EDI-DIA
           MOVE WS-FEC-SIS(3:2)    TO WS-FEC-EDI-MES
           MOVE WS-FEC-SIS(1:2)    TO WS-FEC-EDI-ANO
           MOVE WS-FEC-EDI         TO RSAL-01-FEC-SIS
      * TOMAR FECHA DEL SISTEMA Y MODIFICAR EL FORMATO SOLICITADO
           ACCEPT WS-HOR-SIS       FROM TIME
           MOVE WS-HOR-SIS(1:2)    TO WS-HOR-EDI-HOR
           MOVE WS-HOR-SIS(3:2)    TO WS-HOR-EDI-MIN
           MOVE WS-HOR-SIS(5:2)    TO WS-HOR-EDI-HOR
           MOVE WS-HOR-EDI         TO RSAL-01-HOR-SIS
      * SE GENERA EL ENCABEZADO
           DISPLAY REG-SAL-ENC-01  LINE 01 POSITION 01
                   REG-SAL-ENC-02  LINE 02 POSITION 01
                   REG-SAL-ENC-03  LINE 03 POSITION 01
                   REG-SAL-ENC-02  LINE 04 POSITION 01.
