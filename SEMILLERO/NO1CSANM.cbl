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
       02  WS-SEGURO               PIC A(01) VALUE SPACES.
       01  WS-VALORES PIC 9(07)    VALUE ZEROS.
           88 VAL-BAU              VALUE 0050000.
           88 VAL-PRI              VALUE 0080000.
           88 VAL-CON              VALUE 0020000.
           88 VAL-MAT              VALUE 0250000.
           88 VAL-FUN              VALUE 0300000.
           88 VAL-MIS              VALUE 0060000.
       01  WS-PORCE                PIC 9(03) VALUE 020.
       01  WS-CALCULO              PIC 9(07) VALUE ZEROS.


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
