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
           SELECT SERVICIO ASSIGN TO './FILES/SANMIGUEL/SANMSERV'
           ORGANIZATION IS SEQUENTIAL
      *    CREAR LOS REGISTROS DE LOS SERVICIOS LINEA POR LINEA
      *    ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS VAR-ESTADO.
      * IMPRESION DEL ARCHIVO
           SELECT IMPRESOR ASSIGN TO './FILES/SANMIGUEL/IMPRESORA'
           FILE STATUS IS VAR-ESTADO.
      * CONSECUTIVO DE LOS SERVICIOS DE LA PARROQUIA
           SELECT CONSECUT ASSIGN TO './FILES/SANMIGUEL/CONSECUT'
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
           02 ASER-TELEFONO        PIC 9(10).
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
           02 WS-FEC-EDI-SIG       PIC 9(02) VALUE 20.
           02 WS-FEC-EDI-ANO       PIC 9(02) VALUE ZEROS.

       01  WS-HOR-EDI.
           02 WS-HOR-EDI-HOR       PIC 9(02) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE ':'.
           02 WS-HOR-EDI-MIN       PIC 9(02) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE ':'.
           02 WS-HOR-EDI-SEG       PIC 9(02) VALUE ZEROS.

       01  WS-FEC.
           02 WS-FEC-DIA           PIC 99 VALUE ZEROS.
           02 WS-FEC-MES           PIC 99 VALUE ZEROS.
           02 WS-FEC-ANO           PIC 99 VALUE ZEROS.

       01  SW-FECHA                PIC 9 VALUE ZEROS.
           88 SW-FEC-OK             VALUE 1.
           88 SW-FEC-NOK            VALUE 0.
           
       01  WS-HOR.
           02 WS-HOR-HOR           PIC 99 VALUE ZEROS.
           02 WS-HOR-MIN           PIC 99 VALUE ZEROS.

       01  SW-HORA                 PIC 9 VALUE ZEROS.
           88 SW-HOR-OK            VALUE 1.
           88 SW-HOR-NOK           VALUE 0.

       01  WS-NOM                  PIC X(20) VALUE SPACES.
       01  WS-TEL                  PIC 9(10) VALUE ZEROS.

       01  WS-AGR                  PIC A VALUE SPACES.
           88 WS-SI-AGR            VALUE 'S' 's'.
           88 WS-NO-AGR            VALUE 'N' 'n'.

       01  WS-TOM-SER              PIC A VALUE SPACES.
           88 WS-SI-TOM            VALUE 'S' 's'.
           88 WS-NO-TOM            VALUE 'N' 'n'.

       01  WS-VAL-AGR              PIC 9(07) VALUE ZEROS.
       01  WS-VAL-TOT              PIC 9(07) VALUE ZEROS.
       01  SW-FDA-CONSECUT         PIC 9 VALUE ZEROS.
       01  SW-FDA-SERVICIO         PIC 9 VALUE ZEROS.

       01  WS-ENCONTRAR           PIC A(01) VALUE SPACES.
           88 SW-SI-ENCONTRO      VALUE 'S' 's'.
           88 SW-NO-ENCONTRO      VALUE 'N' 'n'.

       01  WS-NUM-SER-AUX          PIC 9(04) VALUE ZEROS.
       01  WS-ESTA                 PIC A VALUE SPACES.
       01  WS-OPC2                 PIC 9(01) VALUE ZEROS.
       01  WS-EST                  PIC A VALUE SPACES.
           88 ESTADO-PEN           VALUE 'P' 'p'.
           88 ESTADO-REA           VALUE 'R' 'r'.
           88 ESTADO-CAN           VALUE 'C' 'c'.
       01  WS-SERVICIO             PIC 9(01) VALUE ZEROS.
       01  LI                      PIC 9(02) VALUE ZEROS.

      * MASCARAS
       01  WS-MASCARA              PIC $$$,$$9 VALUE ZEROS. 

      * SALIDA
       01  REG-SAL-ENC-01.
           02 FILLER               PIC X(09) VALUE 'FEC.SIS: '.
           02 RSAL-01-FEC-SIS      PIC X(53) VALUE SPACES.
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
           02 FILLER               PIC X(04) VALUE 'SER'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(10) VALUE 'TELEFONO'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(20) VALUE 'NOMBRE'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(10) VALUE 'FECHA'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(05) VALUE 'HORA'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(01) VALUE 'E'.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(07) VALUE 'VALOR'.

      * DETALLES DE PANTALLA # 1
       01  REG-SAL-DET-02.
           02 RSAL-D02-NUM-SER     PIC 9(04) VALUE ZEROS.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 RSAL-D02-COD-SER     PIC X(01) VALUE ZEROS.
           02 FILLER               PIC X(01) VALUE SPACES.
           02 RSAL-D02-NOM-SER     PIC X(02) VALUE SPACES.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 RSAL-D02-TEL-CLI     PIC 9(10) VALUE ZEROS.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 RSAL-D02-NOM-CLI     PIC X(20) VALUE SPACES.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 RSAL-D02-FEC-SER     PIC X(10) VALUE SPACES.
           02 FILLER               PIC X(02) VALUE SPACES.
           02 RSAL-D02-HOR-SER     PIC X(05) VALUE SPACES.
           02 FILLER               PIC X(02) VALUE SPACES.
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
               WHEN 3 PERFORM 1-3-MODIFICA-SERVICIO
               WHEN 4 PERFORM 1-4-BORRA-SERVICIO
               WHEN 5 MOVE 0 TO WS-OPC2 
                      PERFORM 1-5-MENU-CONSULTAS UNTIL WS-OPC2 = 5
               WHEN 6 PERFORM 1-6-IMPRIME-ARCHIVO
           END-EVALUATE.

       1-1-CREA-ARCHIVO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'QUIERES CREAR UN NUEVO ARCHIVO? S/N'
                                   LINE 10 POSITION 01
           MOVE SPACE TO WS-SEGURO
           PERFORM UNTIL SI-SEGURO OR NO-SEGURO
               ACCEPT WS-SEGURO    LINE 10 POSITION 50
           END-PERFORM
           IF SI-SEGURO
               OPEN OUTPUT SERVICIO
               CLOSE SERVICIO
               OPEN OUTPUT CONSECUT
                 MOVE ZEROS TO ACON-CONSECUTIVO
                 WRITE REG-CONSECUT END-WRITE
               CLOSE CONSECUT
               DISPLAY 'ARCHIVOS CREADO CON EXITO, OPRIMA ENTER'
                                   LINE 12 POSITION 01
               ACCEPT WS-ENTER     LINE 12 POSITION 50
           END-IF.

       1-2-ADICIONA-SERVICIO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'ADICION DE SERVICIO' LINE 06 POSITION 31
           PERFORM 1-2-0-CAPTURA-SERVICIO
           PERFORM 1-2-1-CAPTURA-NOMBRE
           PERFORM 1-2-2-CAPTURA-TELEFONO
           PERFORM 1-2-3-CAPTURA-FECHA
           PERFORM 1-2-4-CAPTURA-HORA
           PERFORM 1-2-5-CAPTURA-VALOR
           PERFORM 1-2-6-CAPTURA-AGREGADO
           PERFORM 1-2-7-MOSTRAR-TOTAL
           PERFORM 1-2-8-CAPTURA-TOMA-SER
           IF WS-SI-TOM
               OPEN EXTEND SERVICIO
               IF VAR-ESTADO = '00'
                CONTINUE
               ELSE
                DISPLAY 'ERROR ABRIENDO EL ARCHIVO SERVICIOS'
                                    LINE 23 POSITION 10
                        VAR-ESTADO  LINE 24 POSITION 25
                ACCEPT WS-ENTER     LINE 24 POSITION 30
               END-IF
               PERFORM 1-2-9-GRABAR-SERVICIO
               CLOSE SERVICIO
           END-IF.

       1-2-0-CAPTURA-SERVICIO.
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

       1-2-1-CAPTURA-NOMBRE.
           DISPLAY 'NOMBRE        :                ' LINE 13 POSITION 01
           MOVE SPACES TO WS-NOM
           PERFORM UNTIL WS-NOM > SPACES
               ACCEPT WS-NOM                         LINE 13 POSITION 17
           END-PERFORM.

       1-2-2-CAPTURA-TELEFONO.
           DISPLAY 'TELEFONO      :                ' LINE 13 POSITION 46
           MOVE ZEROS TO WS-TEL
           PERFORM UNTIL WS-TEL > ZEROS
               ACCEPT WS-TEL                         LINE 13 POSITION 56
           END-PERFORM.

       1-2-3-CAPTURA-FECHA.
           DISPLAY 'FECHA EVENTO (DDMMAA) :        ' LINE 14 POSITION 01
           SET SW-FEC-NOK TO TRUE
           PERFORM UNTIL SW-FEC-OK
               ACCEPT WS-FEC                         LINE 14 POSITION 25
               IF WS-FEC-SIS(1:2) = WS-FEC-ANO
                   IF WS-FEC-SIS(3:2) = WS-FEC-MES
                       IF WS-FEC-DIA > WS-FEC-SIS(5:2) AND < 31
                           SET SW-FEC-OK TO TRUE
                       ELSE
                           DISPLAY 'ERROR, DIA INVALIDO' 
                                                     LINE 24 POSITION 30
                           ACCEPT WS-ENTER           LINE 24 POSITION 52
                       END-IF
                    ELSE
                        DISPLAY 'ERROR, MES INVALIDO' 
                                                     LINE 24 POSITION 30
                        ACCEPT WS-ENTER              LINE 24 POSITION 52
                   END-IF
                ELSE
                    DISPLAY 'ERROR, DEBE SER ESTE ANO' 
                                                     LINE 24 POSITION 30
                    ACCEPT WS-ENTER                  LINE 24 POSITION 52
               END-IF
           END-PERFORM.

       1-2-4-CAPTURA-HORA.
           DISPLAY 'HORA EVENTO (HHMM) :          '  LINE 14 POSITION 46
           SET SW-HOR-NOK TO TRUE
           PERFORM UNTIL SW-HOR-OK
               ACCEPT WS-HOR                         LINE 14 POSITION 70
               IF WS-HOR-HOR > 06 AND < 21
                   IF WS-HOR-MIN >= 00 AND < 60
                           SET SW-HOR-OK TO TRUE
                   ELSE
                       DISPLAY 'ERROR, MINUTO INVALIDO' 
                                                     LINE 24 POSITION 30
                       ACCEPT WS-ENTER               LINE 24 POSITION 52
                   END-IF
                 ELSE
                     DISPLAY 'ERROR, HORA INVALIDA' 
                                                     LINE 24 POSITION 30
                     ACCEPT WS-ENTER                 LINE 24 POSITION 52
               END-IF
           END-PERFORM.

       1-2-5-CAPTURA-VALOR.
           DISPLAY 'VALOR DEL SERVICIO :          '  LINE 16 POSITION 01
           EVALUATE WS-SER
               WHEN 1 SET VAL-BAU TO TRUE
               WHEN 2 SET VAL-PRI TO TRUE
               WHEN 3 SET VAL-CON TO TRUE
               WHEN 4 SET VAL-MAT TO TRUE
               WHEN 5 SET VAL-FUN TO TRUE
               WHEN 6 SET VAL-MIS TO TRUE
           END-EVALUATE
           MOVE WS-VALORES TO WS-MASCARA
           DISPLAY WS-MASCARA                       LINE 16 POSITION 25.

       1-2-6-CAPTURA-AGREGADO.
           MOVE ZEROS TO WS-VAL-AGR
           DISPLAY 'DESEA AGREGADOS (20%) (S/N):  '  LINE 17 POSITION 01
           MOVE SPACES TO WS-AGR
           PERFORM UNTIL WS-SI-AGR OR WS-NO-AGR
               ACCEPT WS-AGR                         LINE 17 POSITION 30
           END-PERFORM
           IF WS-SI-AGR
               DISPLAY 'VALOR AGREGADO (20%):      ' LINE 17 POSITION 46
               COMPUTE WS-VAL-AGR = WS-VALORES * (WS-PORCE / 100)
               END-COMPUTE
               MOVE WS-VAL-AGR TO WS-MASCARA
               DISPLAY WS-MASCARA                    LINE 17 POSITION 68
           END-IF.

       1-2-7-MOSTRAR-TOTAL.
           DISPLAY 'TOTAL SERVICIO:             '    LINE 18 POSITION 01
           ADD WS-VALORES WS-VAL-AGR GIVING WS-VAL-TOT
           MOVE WS-VAL-TOT TO WS-MASCARA
           DISPLAY WS-MASCARA                       LINE 18 POSITION 17.

       1-2-8-CAPTURA-TOMA-SER.
           DISPLAY "DESEA TOMAR EL SERVICIO (S/N):" LINE 20 POSITION 29
           MOVE SPACES TO WS-TOM-SER
           PERFORM UNTIL WS-SI-TOM OR WS-NO-TOM
                ACCEPT WS-TOM-SER                   LINE 20 POSITION 60
           END-PERFORM.

       1-2-9-GRABAR-SERVICIO.
           PERFORM 1-2-9-1-OBTIENE-CONSECUTIVO
           PERFORM 1-2-9-2-MOVER-CAMPOS-SERVI
           WRITE REG-SERVICIO END-WRITE.

       1-2-9-1-OBTIENE-CONSECUTIVO.
           OPEN I-O CONSECUT
               READ CONSECUT AT END MOVE 1 TO SW-FDA-CONSECUT
                             NOT AT END ADD 1 TO ACON-CONSECUTIVO
                             REWRITE REG-CONSECUT END-REWRITE
               END-READ
           CLOSE CONSECUT.

       1-2-9-2-MOVER-CAMPOS-SERVI.
           MOVE ACON-CONSECUTIVO TO ASER-NUM-SERVICIO
           MOVE WS-SER           TO ASER-COD-SERVICIO
           MOVE WS-FEC-DIA       TO WS-FEC-EDI-DIA
           MOVE WS-FEC-MES       TO WS-FEC-EDI-MES
           MOVE WS-FEC-ANO       TO WS-FEC-EDI-ANO
           MOVE 20               TO WS-FEC-EDI-SIG
           MOVE WS-FEC-EDI       TO ASER-FECHA
           MOVE WS-HOR-HOR       TO ASER-HORA(1:2)
           MOVE ':'              TO ASER-HORA(3:1)  
           MOVE WS-HOR-MIN       TO ASER-HORA(4:2)
           MOVE WS-NOM           TO ASER-NOMBRE
           MOVE WS-TEL           TO ASER-TELEFONO
           MOVE WS-AGR           TO ASER-AGREGADOS
           MOVE WS-VAL-TOT       TO ASER-VALOR
           MOVE 'P'              TO ASER-ESTADO.

       1-3-MODIFICA-SERVICIO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'MODIFICACION: '         LINE 06 POSITION 01
           PERFORM 1-3-1-CAPTURA-NUM-SERVI
           OPEN I-O SERVICIO 
           SET SW-NO-ENCONTRO TO TRUE
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-3-2-BUSCAR-REGISTRO UNTIL SW-SI-ENCONTRO OR
                                               SW-FDA-SERVICIO = 1
           IF SW-SI-ENCONTRO AND ASER-ESTADO = 'P'
               DISPLAY 'MODIFICACION DE LOS SERIVICIOS' 
                               LINE 06 POSITION 35
               PERFORM 1-3-3-MOSTRAR-REGISTRO
               PERFORM 1-3-4-MODIFICA-REGISTRO
           ELSE
               IF SW-NO-ENCONTRO
                   DISPLAY 'SERVICIO INEXISTENTE' 
                               LINE 24 POSITION 34
                   ACCEPT WS-ENTER  LINE 24 POSITION 50
               ELSE
                    DISPLAY 'ERROR. SERVICIO EN ESTADO CANCELADO/REALIZA
      -                     'DO' 
                                   LINE 24 POSITION 01
                    ACCEPT WS-ENTER  LINE 24 POSITION 50
               END-IF
           END-IF
           CLOSE SERVICIO.
           
       1-3-1-CAPTURA-NUM-SERVI.
           DISPLAY 'CODIGO SERVICIO: ' LINE 07 POSITION 01
           MOVE ZEROS TO WS-NUM-SER-AUX
           PERFORM UNTIL WS-NUM-SER-AUX > ZEROS
               ACCEPT WS-NUM-SER-AUX LINE 07 POSITION 37
           END-PERFORM.

       1-3-2-BUSCAR-REGISTRO.
           READ SERVICIO AT END MOVE 1 TO SW-FDA-SERVICIO
                       NOT AT END PERFORM 1-3-2-1-AVERIGUE
           END-READ.

       1-3-2-1-AVERIGUE.
           IF ASER-NUM-SERVICIO = WS-NUM-SER-AUX
               SET SW-SI-ENCONTRO TO TRUE
           END-IF.

       1-3-3-MOSTRAR-REGISTRO.
           DISPLAY 'NUMERO SERVICIO: ' LINE 08 POSITION 01
                   'SERVICIO         ' LINE 09 POSITION 01
                   ASER-COD-SERVICIO   LINE 09 POSITION 18
           EVALUATE ASER-COD-SERVICIO
               WHEN 1 DISPLAY 'BAUTISMO'         LINE 09 POSITION 20
               WHEN 2 DISPLAY 'PRIMERA COMUNION' LINE 09 POSITION 20
               WHEN 3 DISPLAY 'CONFIRMACION'     LINE 09 POSITION 20
               WHEN 4 DISPLAY 'MATRIMONIO'       LINE 09 POSITION 20
               WHEN 5 DISPLAY 'FUNERALES'        LINE 09 POSITION 20
               WHEN 6 DISPLAY 'MISAS'            LINE 09 POSITION 20
           END-EVALUATE
           DISPLAY 'NOMBRE:'            LINE 10 POSITION 01
           DISPLAY ASER-NOMBRE          LINE 10 POSITION 18
           DISPLAY 'TELEFONO:'          LINE 10 POSITION 56
           DISPLAY ASER-TELEFONO        LINE 10 POSITION 67
           DISPLAY 'CON AGREGADOS:'     LINE 11 POSITION 01
           DISPLAY ASER-AGREGADOS       LINE 11 POSITION 18
           DISPLAY 'FECHA EVENTO:'      LINE 12 POSITION 01
           DISPLAY ASER-FECHA           LINE 12 POSITION 18
           DISPLAY 'HORA EVENTO:'       LINE 12 POSITION 56
           DISPLAY ASER-HORA            LINE 12 POSITION 70
           DISPLAY 'TOTAL SERVICIO:'    LINE 13 POSITION 01
           MOVE ASER-VALOR TO WS-MASCARA
           DISPLAY WS-MASCARA          LINE 13 POSITION 18
           DISPLAY 'ESTADO:'           LINE 14 POSITION 01
           DISPLAY ASER-ESTADO         LINE 14 POSITION 18.

       1-3-4-MODIFICA-REGISTRO.
           ACCEPT ASER-NOMBRE   WITH UPDATE   LINE 10 POSITION 18
           ACCEPT ASER-TELEFONO WITH UPDATE   LINE 10 POSITION 67
           MOVE ASER-ESTADO TO WS-ESTA
           ACCEPT ASER-ESTADO   WITH UPDATE   LINE 14 POSITION 18
           IF ASER-ESTADO = WS-ESTA
               CONTINUE
           ELSE
               PERFORM UNTIL ASER-ESTADO = 'R' OR 'P'
               ACCEPT ASER-ESTADO WITH UPDATE LINE 14 POSITION 18 
               END-PERFORM
           END-IF
           REWRITE REG-SERVICIO END-REWRITE.

       1-4-BORRA-SERVICIO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'BORRADO: '         LINE 06 POSITION 01
           PERFORM 1-3-1-CAPTURA-NUM-SERVI
           OPEN I-O SERVICIO 
           SET SW-NO-ENCONTRO TO TRUE
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-3-2-BUSCAR-REGISTRO UNTIL SW-SI-ENCONTRO OR
                                               SW-FDA-SERVICIO = 1
           IF SW-SI-ENCONTRO AND ASER-ESTADO = 'P'
               DISPLAY 'ELIMINACION DE LOS SERVICIOS' 
                               LINE 06 POSITION 35
               PERFORM 1-3-3-MOSTRAR-REGISTRO
               DISPLAY 'ESTA SEGURO DE BORRARLO (S/N)' 
                               LINE 24 POSITION 25
               MOVE SPACES TO WS-SEGURO
               PERFORM UNTIL SI-SEGURO OR NO-SEGURO
               ACCEPT WS-SEGURO LINE 24 POSITION 60 
               END-PERFORM
               IF SI-SEGURO
                   MOVE 'C' TO ASER-ESTADO
                   REWRITE REG-SERVICIO END-REWRITE
               END-IF
           ELSE
               IF SW-NO-ENCONTRO
                   DISPLAY 'SERVICIO INEXISTENTE' 
                               LINE 24 POSITION 34
                   ACCEPT WS-ENTER  LINE 24 POSITION 50
               ELSE
                    DISPLAY 'ERROR. SERVICIO EN ESTADO CANCELADO/REALIZA
      -                     'DO' 
                                   LINE 24 POSITION 01
                    ACCEPT WS-ENTER  LINE 24 POSITION 50
               END-IF
           END-IF
           CLOSE SERVICIO.

       1-5-MENU-CONSULTAS.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'QUE TIPO DE CONSULTA DESEA REALIZAR?: '
                                            LINE 06 POSITION 21
                   '1. NUMERO DE SERVICIO ' LINE 07 POSITION 10
                   '2. ESTADO '             LINE 08 POSITION 10
                   '3. SERVICIOS '          LINE 09 POSITION 10
                   '4. TODOS '              LINE 10 POSITION 10
                   '5. SALIR '              LINE 11 POSITION 10
                   'OPCION ) '              LINE 12 POSITION 10
           MOVE ZEROS TO WS-OPC2
           PERFORM UNTIL WS-OPC2 > 0 AND < 6
               ACCEPT WS-OPC2             LINE 12 POSITION 20
           END-PERFORM
           EVALUATE WS-OPC2
               WHEN 1 PERFORM 1-5-1-CONSU-NUM-SERVI
               WHEN 2 PERFORM 1-5-2-CONSU-POR-ESTADO
               WHEN 3 PERFORM 1-5-3-CONSU-POR-SERVICIOS
               WHEN 4 PERFORM 1-5-4-CONSU-TODO-ARCHIVO
           END-EVALUATE.

       1-5-1-CONSU-NUM-SERVI.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 1-3-1-CAPTURA-NUM-SERVI
           OPEN INPUT SERVICIO 
           SET SW-NO-ENCONTRO TO TRUE
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-3-2-BUSCAR-REGISTRO UNTIL SW-SI-ENCONTRO OR
                                               SW-FDA-SERVICIO = 1
           IF SW-SI-ENCONTRO
               PERFORM 1-3-3-MOSTRAR-REGISTRO
               DISPLAY 'OPRIMA ENTER PARA SALIR:' 
                               LINE 24 POSITION 27
               ACCEPT WS-ENTER  LINE 24 POSITION 50
           ELSE
               DISPLAY 'SERVICIO INEXISTENTE, ENTER PARA SALIR:'
                                LINE 24 POSITION 34
               ACCEPT WS-ENTER  LINE 24 POSITION 50
           END-IF
           CLOSE SERVICIO.

       1-5-2-CONSU-POR-ESTADO.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 1-5-2-1-CAPTURA-ESTADO
           PERFORM 1-5-2-2-TITULO-DETA-PAN
           MOVE 7 TO LI
           OPEN I-O SERVICIO
           SET SW-NO-ENCONTRO TO TRUE
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-5-2-3-BUSCA-ESTADOS UNTIL SW-FDA-SERVICIO = 1
           PERFORM 999-ENTER
           CLOSE SERVICIO.

       1-5-2-1-CAPTURA-ESTADO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'QUE TIPO DE ESTADO DESEA CONSULTAR?: '
                                            LINE 06 POSITION 21
                   'P. PENDIENTES '         LINE 07 POSITION 10
                   'R. REALIZADOS '         LINE 08 POSITION 10
                   'C. CANCELADOS '         LINE 09 POSITION 10
                   'OPCION ) '              LINE 11 POSITION 10
           MOVE SPACES TO WS-EST
           PERFORM UNTIL ESTADO-PEN OR ESTADO-REA OR ESTADO-CAN
               ACCEPT WS-EST                LINE 11 POSITION 20
           END-PERFORM
           INSPECT WS-EST CONVERTING 'prc' TO 'PRC'.

       1-5-2-2-TITULO-DETA-PAN.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY REG-SAL-DET-01           LINE 06 POSITION 01.

       1-5-2-3-BUSCA-ESTADOS.
           READ SERVICIO AT END MOVE 1 TO SW-FDA-SERVICIO
                         NOT AT END PERFORM 1-5-2-3-1-AVERIGUA-ESTADO
           END-READ.

       1-5-2-3-1-AVERIGUA-ESTADO.
           IF ASER-ESTADO = WS-EST
               SET SW-SI-ENCONTRO TO TRUE
               PERFORM 1-5-3-1-1-MUESTRE-REGISTRO
               DISPLAY REG-SAL-DET-02      LINE LI POSITION 01
               ADD 1 TO LI
               IF LI = 23
                    PERFORM 999-ENCABEZADO-PAN
                    PERFORM 1-5-2-2-TITULO-DETA-PAN
                    MOVE 7 TO LI
                    PERFORM 999-ENTER
           END-IF.

       1-5-3-1-1-MUESTRE-REGISTRO.
           MOVE ASER-NUM-SERVICIO      TO RSAL-D02-NUM-SER
           MOVE ASER-COD-SERVICIO      TO RSAL-D02-COD-SER
           EVALUATE ASER-COD-SERVICIO
               WHEN 1 MOVE 'BA' TO RSAL-D02-NOM-SER
               WHEN 2 MOVE 'PR' TO RSAL-D02-NOM-SER
               WHEN 3 MOVE 'CO' TO RSAL-D02-NOM-SER
               WHEN 4 MOVE 'MA' TO RSAL-D02-NOM-SER
               WHEN 5 MOVE 'FU' TO RSAL-D02-NOM-SER
               WHEN 6 MOVE 'MI' TO RSAL-D02-NOM-SER
           END-EVALUATE
           MOVE 'BA'                   TO RSAL-D02-NOM-SER
           MOVE ASER-TELEFONO          TO RSAL-D02-TEL-CLI
           MOVE ASER-NOMBRE            TO RSAL-D02-NOM-CLI
           MOVE ASER-FECHA             TO RSAL-D02-FEC-SER
           MOVE ASER-HORA              TO RSAL-D02-HOR-SER
           MOVE ASER-ESTADO            TO RSAL-D02-EST-SER
           MOVE ASER-VALOR             TO RSAL-D02-VAL-SER.

       1-5-3-CONSU-POR-SERVICIOS.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 1-2-0-CAPTURA-SERVICIO
           PERFORM 1-5-2-2-TITULO-DETA-PAN
           MOVE 7 TO LI
           OPEN INPUT SERVICIO
           SET SW-NO-ENCONTRO TO TRUE
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-5-3-1-BUSCA-SERVICIOS UNTIL SW-FDA-SERVICIO = 1
           PERFORM 999-ENTER
           CLOSE SERVICIO.

       1-5-3-1-BUSCA-SERVICIOS.
           READ SERVICIO AT END MOVE 1 TO SW-FDA-SERVICIO
                         NOT AT END PERFORM 1-5-3-1-1-AVERIGUA-SERVICI
           END-READ.

       1-5-3-1-1-AVERIGUA-SERVICI.
           IF ASER-COD-SERVICIO = WS-SER
               SET SW-SI-ENCONTRO TO TRUE
               PERFORM 1-5-3-1-1-MUESTRE-REGISTRO
               DISPLAY REG-SAL-DET-02      LINE LI POSITION 01
               ADD 1 TO LI
               IF LI = 23
                    MOVE 7 TO LI
                    PERFORM 999-ENTER
                    PERFORM 999-ENCABEZADO-PAN
                    PERFORM 1-5-2-2-TITULO-DETA-PAN
           END-IF.

       1-5-4-CONSU-TODO-ARCHIVO.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 1-5-2-2-TITULO-DETA-PAN
           MOVE 7 TO LI
           OPEN INPUT SERVICIO
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-5-4-1-BUSCA-TODOS UNTIL SW-FDA-SERVICIO = 1
           PERFORM 999-ENTER
           CLOSE SERVICIO.

       1-5-4-1-BUSCA-TODOS.
           READ SERVICIO AT END MOVE 1 TO SW-FDA-SERVICIO
                         NOT AT END PERFORM 1-5-4-1-1-MUESTRA-TODOS
           END-READ.

       1-5-4-1-1-MUESTRA-TODOS.
           PERFORM 1-5-3-1-1-MUESTRE-REGISTRO
           DISPLAY REG-SAL-DET-02      LINE LI POSITION 01
           ADD 1 TO LI
           IF LI = 23
                MOVE 7 TO LI
                PERFORM 999-ENTER
                PERFORM 999-ENCABEZADO-PAN
                PERFORM 1-5-2-2-TITULO-DETA-PAN
           END-IF.

       1-6-IMPRIME-ARCHIVO.
           OPEN OUTPUT IMPRESOR
           PERFORM 1-6-1-ENCABEZADO-IMP
           PERFORM 1-6-2-TITULO-DETA-IMP
           MOVE 7 TO LI
           OPEN INPUT SERVICIO
           MOVE 0 TO SW-FDA-SERVICIO
           PERFORM 1-6-3-IMPRIME-TODOS UNTIL SW-FDA-SERVICIO = 1
           PERFORM 999-ENTER
           CLOSE SERVICIO IMPRESOR.

       1-6-1-ENCABEZADO-IMP.
           DISPLAY CLEAR-SCREEN
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
           MOVE WS-HOR-SIS(5:2)    TO WS-HOR-EDI-SEG
           MOVE WS-HOR-EDI         TO RSAL-01-HOR-SIS
      * SE GENERA EL ENCABEZADO
           WRITE REG-IMPRESOR FROM REG-SAL-ENC-01 AFTER PAGE END-WRITE
           WRITE REG-IMPRESOR FROM REG-SAL-ENC-02 AFTER 1 END-WRITE
           WRITE REG-IMPRESOR FROM REG-SAL-ENC-03 AFTER 1 END-WRITE
           WRITE REG-IMPRESOR FROM REG-SAL-ENC-02 AFTER 1 END-WRITE.

       1-6-2-TITULO-DETA-IMP.
      *    PERFORM 1-6-1-ENCABEZADO-IMP
           WRITE REG-IMPRESOR FROM REG-SAL-DET-01 AFTER 1 END-WRITE.

       1-6-3-IMPRIME-TODOS.
            READ SERVICIO AT END MOVE 1 TO SW-FDA-SERVICIO
                          NOT AT END PERFORM 1-6-3-1-IMPRIMA-REGISTRO
            END-READ.

       1-6-3-1-IMPRIMA-REGISTRO.
           PERFORM 1-5-3-1-1-MUESTRE-REGISTRO
           WRITE REG-IMPRESOR FROM REG-SAL-DET-02 AFTER 1 END-WRITE
           ADD 1 TO LI
           IF LI = 60
                PERFORM 1-6-2-TITULO-DETA-IMP
                MOVE 7 TO LI
           END-IF.

       999-ENTER.
           DISPLAY 'IMPRESION REALIZADA CON EXITO' 
                       LINE 24 POSITION 27
           ACCEPT WS-ENTER  LINE 24 POSITION 55.

       999-ENCABEZADO-PAN.
           DISPLAY CLEAR-SCREEN
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
           MOVE WS-HOR-SIS(5:2)    TO WS-HOR-EDI-SEG
           MOVE WS-HOR-EDI         TO RSAL-01-HOR-SIS
      * SE GENERA EL ENCABEZADO
           DISPLAY REG-SAL-ENC-01  LINE 01 POSITION 01
                   REG-SAL-ENC-02  LINE 02 POSITION 01
                   REG-SAL-ENC-03  LINE 03 POSITION 01
                   REG-SAL-ENC-02  LINE 04 POSITION 01.
