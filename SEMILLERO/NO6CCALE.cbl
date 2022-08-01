      *----------------------------------------------------------------*
      * OBJETIVO: CREAR UN APLICATIVO QUE VALIDE LA DISPONIBILIDAD     *
      * PARA SOLICITAR CUPOS EN LA IGLESIA                             *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                  NO5CCALE.
       AUTHOR.                      NOVATEC SOLUTIONS (EDWIN-PAEZ).
       INSTALLATION.                PARROQUIA SAN MIGUEL.
       DATE-WRITTEN.                29-07-22.
      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * ARCHIVO QUE GUARDARA LA INFORMACION DE LOS DIAS Y HORAS
           SELECT DATOSCAL ASSIGN TO './FILES/CALENDARIO/DATOSCAL'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS VAR-ESTADO.
      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  DATOSCAL LABEL RECORD STANDARD
           RECORDING MODE IS FIXED
           BLOCK CONTAINS 0 RECORDS.
       01  REG-CALENDARIO.
           02 REG-FECHA             PIC 9(06).
           02 REG-HORA              PIC 9(02).
           02 REG-ESTADO            PIC A(01).
           
       WORKING-STORAGE SECTION.
      *                           UTILIDADES                           *
       01  WS-ENTER                 PIC X(01) VALUE SPACES.
       01  VAR-ESTADO               PIC A(02) VALUE SPACES.

      *                           SALIDAS                              *
      * FECHA Y HORA
       01  WS-FEC-SIS               PIC 9(06) VALUE ZEROS.
       01  WS-HOR-SIS               PIC 9(08) VALUE ZEROS.
       01  REG-SAL-ENC-01.
           02 FILLER                PIC X(09) VALUE 'FEC-SIS: '.
           02 RSAL-01-FEC-SIS       PIC X(53) VALUE SPACES.
           02 FILLER                PIC X(10) VALUE 'HORA-SIS: '.
           02 RSAL-01-HOR-SIS       PIC X(08) VALUE SPACES.

       01  REG-SAL-ENC-02.
           02 FILLER                PIC X(80) VALUE ALL '*'.

       01  REG-SAL-ENC-03.
           02 FILLER                PIC X(24) VALUE SPACES.
           02 FILLER                PIC X(31) VALUE 'CALENDARIO PARROQUI
      -                                            'A SAN MIGUEL'.
           02 FILLER                PIC X(25) VALUE SPACES.

       01  REG-SAL-DIA.
           02 FILLER                PIC X(10) VALUE SPACES.
           02 FILLER                PIC X(05) VALUE ALL 'FECHA'.
           02 FILLER                PIC X(10) VALUE SPACES.
           02 FILLER                PIC X(04) VALUE ALL 'HORA'.
           02 FILLER                PIC X(10) VALUE SPACES.
           02 FILLER                PIC X(06) VALUE ALL 'ESTADO'.

      *                           PROCESOS                             *
       01  WS-CREAR                 PIC A(01) VALUE SPACES.
           88 SI-CREAR              VALUE 'S' 's'.
           88 NO-CREAR              VALUE 'N' 'n'.
       
       01  WS-FIN-ARCHIVO           PIC 9(01) VALUE ZEROS.
       01  WS-OPC                   PIC 9(01) VALUE ZEROS.
       01  WS-INHA                  PIC 9(01) VALUE ZEROS.
       01  WS-HORA                  PIC 9(02) VALUE ZEROS.  
       01  WS-DIA                   PIC 9(02) VALUE ZEROS.  
       01  WS-MES                   PIC 9(02) VALUE ZEROS.  

      * VARIABLES PARA EL FOR ANIDADO DE 3 NIVELES, SIN VALIDAR EL AÑO 
      * YA QUE SERÁ TOMATDO AUTOMÁTICAMENTE POR EL SISTEMA
      * M: MESES, D: DIAS, H: HORAS.
       01  M                        PIC 9(02) VALUE ZEROS.
           88 M-30                  VALUE '04' '06' '09' '11'.
           88 M-31                  VALUE '01' '03' '05' '07' '08' '10'
                                    '12'.
           88 M-28                  VALUE '02'.
       01  D                        PIC 9(02) VALUE ZEROS.
       01  H                        PIC 9(02) VALUE ZEROS. 
       01  LI                       PIC 9(02) VALUE ZEROS.

       01  WS-M                     PIC 9(02) VALUE ZEROS.
       01  ANIO                     PIC 9(04) VALUE ZEROS.
       01  WS-R                     PIC 9(02) VALUE ZEROS.
       01  WS-RTA                   PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.
      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-MENU-PRINCIPAL UNTIL WS-OPC = 4
           STOP RUN.

       1000-MENU-PRINCIPAL.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'MENU PRINCIPAL            '     LINE 06 POSITION 33
                   '1. CREAR ARCHIVO          '     LINE 08 POSITION 10
                   '2. MODIFICAR - INHABILITAR'     LINE 09 POSITION 10
                   '3. CONSULTAS              '     LINE 10 POSITION 10
                   '4. SALIR                  '     LINE 11 POSITION 10
                   'OPCION )                  '     LINE 12 POSITION 20
           MOVE ZEROS TO WS-OPC
           PERFORM UNTIL WS-OPC > 0 AND < 5
               ACCEPT WS-OPC                        LINE 12 POSITION 30
           END-PERFORM
           EVALUATE WS-OPC
               WHEN 1 PERFORM 1000-1-CREA-ARCHIVO
               WHEN 2 MOVE ZEROS TO WS-INHA
                      PERFORM 1000-2-MENU-INHABILITAR UNTIL WS-INHA = 4
               WHEN 3 PERFORM 1000-3-1-CONSULTAR-DIA
           END-EVALUATE.

       1000-1-CREA-ARCHIVO.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'SE CREARA UN ARCHIVO NUEVO, PERDERA TODA LA INFORMAC
      -            'ION ALOJADA. '
                                                    LINE 06 POSITION 01
                   'DESEA CONTINUAR? (S/N):'        LINE 07 POSITION 01
           MOVE SPACES TO WS-CREAR
           PERFORM UNTIL SI-CREAR OR NO-CREAR
               ACCEPT WS-CREAR                      LINE 07 POSITION 25
           END-PERFORM
           IF SI-CREAR
             OPEN OUTPUT DATOSCAL
             PERFORM 1000-1-1-GENERAR-INFORMACION
             CLOSE DATOSCAL
             DISPLAY 'ARCHIVO CREADO CON EXITO!' 
                                                    LINE 12 POSITION 23
             PERFORM 999-ENTER
           END-IF.

       1000-1-1-GENERAR-INFORMACION.
           PERFORM 1000-1-1-1-MESES VARYING M FROM 01 BY 01 
                   UNTIL M > 12.

       1000-1-1-1-MESES.
           IF M-30 
             MOVE 30 TO WS-M
           ELSE
             IF M-31
               MOVE 31 TO WS-M
             ELSE
               MOVE 20                 TO ANIO(1:2)
               MOVE WS-FEC-SIS(1:2)    TO ANIO(3:2)
               DIVIDE ANIO BY 4 GIVING WS-RTA REMAINDER WS-R END-DIVIDE
               IF WS-R = 0
                 MOVE 29 TO WS-M
               ELSE
                 MOVE 28 TO WS-M
               END-IF
             END-IF
           END-IF
           PERFORM VARYING D FROM 01 BY 01 UNTIL D > WS-M
               AFTER H FROM 06 BY 01 UNTIL H > 20
                 MOVE D               TO REG-FECHA(1:2)
                 MOVE M               TO REG-FECHA(3:2)
                 MOVE WS-FEC-SIS(1:2) TO REG-FECHA(5:2)
                 MOVE H               TO REG-HORA
                 MOVE SPACES          TO REG-ESTADO
                 WRITE REG-CALENDARIO
           END-PERFORM. 

       1000-2-MENU-INHABILITAR.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'INHABILITAR'                    LINE 06 POSITION 34
                   '1. MES     '                    LINE 08 POSITION 10
                   '2. DIA     '                    LINE 09 POSITION 10
                   '3. HORAS   '                    LINE 10 POSITION 10
                   '4. SALIR   '                    LINE 11 POSITION 10
                   'OPCION )   '                    LINE 12 POSITION 20
           MOVE ZEROS TO WS-INHA
           PERFORM UNTIL WS-INHA > 0 AND < 5
               ACCEPT WS-INHA                       LINE 12 POSITION 30
           END-PERFORM
           EVALUATE WS-INHA
               WHEN 1 PERFORM 1000-2-1-INHABILITAR-MES
               WHEN 2 PERFORM 1000-2-2-INHABILITAR-DIA
               WHEN 3 PERFORM 1000-2-3-INHABILITAR-HORAS
           END-EVALUATE.

       1000-2-1-INHABILITAR-MES.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 999-SOLICITAR-MES
           MOVE 0 TO WS-FIN-ARCHIVO
           OPEN I-O DATOSCAL
           PERFORM 1000-2-1-1-LEER-ARCHIVO-MES UNTIL WS-FIN-ARCHIVO = 1
           CLOSE DATOSCAL
           DISPLAY 'REGISTROS ACTUALIZADOS CON EXITO'
                                                    LINE 22 POSITION 24
           PERFORM 999-ENTER.

       1000-2-1-1-LEER-ARCHIVO-MES.
           READ DATOSCAL AT END MOVE 1 TO WS-FIN-ARCHIVO
                         NOT AT END PERFORM 1000-2-1-2-MODIFICAR-MES
           END-READ.

       1000-2-1-2-MODIFICAR-MES.
           IF REG-FECHA(3:2) = WS-MES
             MOVE 'I' TO REG-ESTADO
             REWRITE REG-CALENDARIO END-REWRITE
           END-IF.
 
       1000-2-2-INHABILITAR-DIA.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 999-SOLICITAR-DIA
           PERFORM 999-SOLICITAR-MES
           MOVE 0 TO WS-FIN-ARCHIVO
           OPEN I-O DATOSCAL
           PERFORM 1000-2-2-1-LEER-ARCHIVO-DIA UNTIL WS-FIN-ARCHIVO = 1
           CLOSE DATOSCAL
           DISPLAY 'REGISTROS ACTUALIZADOS CON EXITO'
                                                    LINE 22 POSITION 24
           PERFORM 999-ENTER.

           1000-2-2-1-LEER-ARCHIVO-DIA.
           READ DATOSCAL AT END MOVE 1 TO WS-FIN-ARCHIVO
                         NOT AT END PERFORM 1000-2-2-2-MODIFICAR-DIA
           END-READ.

       1000-2-2-2-MODIFICAR-DIA.
           IF REG-FECHA(1:2) = WS-DIA AND REG-FECHA(3:2) = WS-MES
             MOVE 'I' TO REG-ESTADO
             REWRITE REG-CALENDARIO END-REWRITE
           END-IF.

       1000-2-3-INHABILITAR-HORAS.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 999-SOLICITAR-HORA
           PERFORM 999-SOLICITAR-DIA
           PERFORM 999-SOLICITAR-MES
           MOVE 0 TO WS-FIN-ARCHIVO
           OPEN I-O DATOSCAL
           PERFORM 1000-2-3-1-LEER-ARCHIVO-HORA UNTIL WS-FIN-ARCHIVO = 1
           CLOSE DATOSCAL
           DISPLAY 'REGISTROS ACTUALIZADOS CON EXITO'
                                                    LINE 22 POSITION 24
           PERFORM 999-ENTER.

           1000-2-3-1-LEER-ARCHIVO-HORA.
           READ DATOSCAL AT END MOVE 1 TO WS-FIN-ARCHIVO
                         NOT AT END PERFORM 1000-2-3-2-MODIFICAR-HORA
           END-READ.

       1000-2-3-2-MODIFICAR-HORA.
           IF REG-FECHA(1:2) = WS-DIA AND REG-FECHA(3:2) = 
              WS-MES AND REG-HORA = WS-HORA
              MOVE 'I' TO REG-ESTADO
              REWRITE REG-CALENDARIO END-REWRITE
           END-IF.

       1000-3-1-CONSULTAR-DIA.
           PERFORM 999-ENCABEZADO-PAN
           PERFORM 999-SOLICITAR-DIA
           PERFORM 999-SOLICITAR-MES
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY REG-SAL-DIA      LINE 06 POSITION 01
           MOVE 0 TO WS-FIN-ARCHIVO
           MOVE 7 TO LI
           OPEN INPUT DATOSCAL
           PERFORM 1000-3-1-1-LEER-DIA UNTIL WS-FIN-ARCHIVO = 1
           CLOSE DATOSCAL
           PERFORM 999-ENTER.

       1000-3-1-1-LEER-DIA.
           READ DATOSCAL AT END MOVE 1 TO WS-FIN-ARCHIVO
                         NOT AT END PERFORM 1000-3-1-1-IMP-DIA
           END-READ.

       1000-3-1-1-IMP-DIA.
           IF REG-FECHA(1:2) = WS-DIA AND REG-FECHA(3:2) = WS-MES
             DISPLAY REG-FECHA(1:2)       LINE LI POSITION 11
             DISPLAY '/'                  LINE LI POSITION 13   
             DISPLAY REG-FECHA(3:2)       LINE LI POSITION 14
             DISPLAY '/'                  LINE LI POSITION 16   
             DISPLAY REG-FECHA(5:2)       LINE LI POSITION 17
             DISPLAY REG-HORA             LINE LI POSITION 26
             DISPLAY ':00'                LINE LI POSITION 28 
             IF REG-ESTADO = 'I'
               DISPLAY 'INHABILITADO'     LINE LI POSITION 40
             END-IF
             ADD 1 TO LI
           END-IF.

       999-SOLICITAR-HORA.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'SELLECCIONE LA HORA (06/20)'    LINE 06 POSITION 34
                   'HORA) '                         LINE 07 POSITION 10
           MOVE ZEROS TO WS-HORA
           PERFORM UNTIL WS-HORA > 5 AND < 21
             ACCEPT WS-HORA                    LINE 07 POSITION 16
           END-PERFORM.

       999-SOLICITAR-DIA.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'SELECCIONE EL DIA (1/30)'      LINE 06 POSITION 34
                   'DIA) '                         LINE 07 POSITION 10
           MOVE ZEROS TO WS-DIA
           PERFORM UNTIL WS-DIA > 0 AND < 31
             ACCEPT WS-DIA                    LINE 07 POSITION 16
           END-PERFORM.

       999-SOLICITAR-MES.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'SELECCIONE EL MES'              LINE 06 POSITION 34
                   '01. ENERO     '                 LINE 08 POSITION 10
                   '02. FEBRERO   '                 LINE 08 POSITION 30
                   '03. MARZO     '                 LINE 08 POSITION 50
                   '04. ABRIL     '                 LINE 09 POSITION 10
                   '05. MAYO      '                 LINE 09 POSITION 30
                   '06. JUNIO     '                 LINE 09 POSITION 50
                   '07. JULIO     '                 LINE 10 POSITION 10
                   '08. AGOSTO    '                 LINE 10 POSITION 30
                   '09. SEPTIEMBRE'                 LINE 10 POSITION 50
                   '10. OCTUBRE   '                 LINE 11 POSITION 10
                   '11. NOVIEMBRE '                 LINE 11 POSITION 30
                   '12. DICIEMBRE '                 LINE 11 POSITION 50
                   'OPCION )      '                 LINE 13 POSITION 20
           MOVE ZEROS TO WS-MES
           PERFORM UNTIL WS-MES > 0 AND < 13
             ACCEPT WS-MES                    LINE 13 POSITION 30
           END-PERFORM.
           
       999-ENCABEZADO-PAN.
           DISPLAY CLEAR-SCREEN
      * TOMAR FECHA DEL SISTEMA Y MODIFICAR EL FORMATO SOLICITADO
           ACCEPT WS-FEC-SIS       FROM DATE
           MOVE WS-FEC-SIS(5:2)    TO RSAL-01-FEC-SIS(1:2)
           MOVE '/'                TO RSAL-01-FEC-SIS(3:1)
           MOVE WS-FEC-SIS(3:2)    TO RSAL-01-FEC-SIS(4:2)
           MOVE '/'                TO RSAL-01-FEC-SIS(6:1)
           MOVE 20                 TO RSAL-01-FEC-SIS(7:2)
           MOVE WS-FEC-SIS(1:2)    TO RSAL-01-FEC-SIS(9:2)
      * TOMAR FECHA DEL SISTEMA Y MODIFICAR EL FORMATO SOLICITADO
           ACCEPT WS-HOR-SIS       FROM TIME
           MOVE WS-HOR-SIS(1:2)    TO RSAL-01-HOR-SIS(1:2)
           MOVE ':'                TO RSAL-01-HOR-SIS(3:1)
           MOVE WS-HOR-SIS(3:2)    TO RSAL-01-HOR-SIS(4:2)
           MOVE ':'                TO RSAL-01-HOR-SIS(6:1)
           MOVE WS-HOR-SIS(5:2)    TO RSAL-01-HOR-SIS(7:2)
      * SE GENERA EL ENCABEZADO
           DISPLAY REG-SAL-ENC-01  LINE 01 POSITION 01
                   REG-SAL-ENC-02  LINE 02 POSITION 01
                   REG-SAL-ENC-03  LINE 03 POSITION 01
                   REG-SAL-ENC-02  LINE 04 POSITION 01.

       999-ENTER.
           DISPLAY 'PRESIONE ENTER PARA CONTINUAR'  LINE 24 POSITION 27
           ACCEPT WS-ENTER                          LINE 24 POSITION 57.
