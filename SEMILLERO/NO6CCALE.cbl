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
           ORGANIZATION IS LINE SEQUENTIAL
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

      *                           PROCESOS                             *
       01  WS-OPC                   PIC 9(01) VALUE ZEROS.
       01  WS-CREAR                 PIC A(01) VALUE SPACES.
           88 SI-CREAR              VALUE 'S' 's'.
           88 NO-CREAR              VALUE 'N' 'n'.

      * VARIABLES PARA EL FOR ANIDADO DE 3 NIVELES, SIN VALIDAR EL AÑO 
      * YA QUE SERÁ TOMATDO AUTOMÁTICAMENTE POR EL SISTEMA
      * M: MESES, D: DIAS, H: HORAS.
       01  M                        PIC 9(02) VALUE ZEROS.
       01  D                        PIC 9(02) VALUE ZEROS.
       01  H                        PIC 9(02) VALUE ZEROS. 
       01  LI                       PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.
      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      * RUTINA DE FECHAS
           PERFORM 1000-MENU-PRINCIPAL
           STOP RUN.

       1000-MENU-PRINCIPAL.
           PERFORM 999-ENCABEZADO-PAN
           DISPLAY 'MENU PRINCIPAL       '          LINE 06 POSITION 33
                   '1. CREAR ARCHIVO     '          LINE 08 POSITION 10
                   '2. INHABILITAR'                 LINE 09 POSITION 10
                   '3. CONSULTAS         '          LINE 10 POSITION 10
                   '4. SALIR             '          LINE 11 POSITION 10
                   'OPCION ) '                      LINE 12 POSITION 20
           MOVE ZEROS TO WS-OPC
           PERFORM UNTIL WS-OPC > 0 AND < 5
               ACCEPT WS-OPC                        LINE 12 POSITION 30
           END-PERFORM
           EVALUATE WS-OPC
               WHEN 1 PERFORM 1000-1-CREA-ARCHIVO
      *        WHEN 2 PERFORM 1000-2-MENU-INHABILITAR
      *        WHEN 3 PERFORM 1000-3-MENU-CONSULTAS
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
           PERFORM VARYING M FROM 01 BY 01 UNTIL M > 12
             AFTER D FROM 01 BY 01 UNTIL D > 30
               AFTER H FROM 06 BY 01 UNTIL H > 20
                 MOVE D               TO REG-FECHA(1:2)
                 MOVE M               TO REG-FECHA(3:2)
                 MOVE WS-FEC-SIS(1:2) TO REG-FECHA(5:2)
                 MOVE H               TO REG-HORA
                 MOVE SPACES          TO REG-ESTADO
                 WRITE REG-CALENDARIO
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
