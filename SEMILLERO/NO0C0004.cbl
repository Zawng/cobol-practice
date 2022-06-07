      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * [X] GENERAR FECHA Y HORA DEL SISTEMA
      * [X] RECIBIR INFORMACION NOMBRE, CEDULA, FECHA NACIMIENTO
      * [ ] MOSTRAR EN PANTALLA FECHA, HORA, USUARIO: CUANTOS AÑOS TIENE
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
           02 WS-EDAD                          PIC 9(18) VALUE ZEROES.
           02 WS-NOMBRE-CLI                    PIC X(30) VALUE SPACES.
           02 WS-CEDULA-CLI                    PIC 9(10) VALUE ZEROES.
           02 WS-FECHA-CLI.
              03 WS-DIA-CLI                    PIC 9(02) VALUE ZEROES. 
              03 FILLER                        PIC X     VALUE '/'.
              03 WS-MES-CLI                    PIC 9(02) VALUE ZEROES.
              03 FILLER                        PIC X     VALUE '/'.
              03 WS-ANO-CLI                    PIC 9(04) VALUE ZEROES.
           
      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
           02 WS-FECHA-ACT                     PIC 9(06) VALUE ZEROES.
           02 WS-HORA-ACT                      PIC 9(08) VALUE ZEROES.
           02 WS-FECHA-SIS.
              03 WS-DIA-SIS                    PIC 9(02) VALUE ZEROES.
              03 FILLER                        PIC X(01) VALUE '/'.
              03 WS-MES-SIS                    PIC 9(02) VALUE ZEROES.
              03 FILLER                        PIC X(01) VALUE '/'.
              03 WS-SIG-SIS                    PIC 9(02) VALUE 20.
              03 WS-ANO-SIS                    PIC 9(04) VALUE ZEROES.

           02 WS-HORA-SIS.
              03 WS-HOR-SIS                    PIC 9(02) VALUE ZEROES.
              03 FILLER                        PIC X(01) VALUE ':'.
              03 WS-MIN-SIS                    PIC 9(02) VALUE ZEROES.
              03 FILLER                        PIC X(01) VALUE ':'.
              03 WS-SEG-SIS                    PIC 9(02) VALUE ZEROES.
       
       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-FECHAS
       PERFORM 2002-INFORMACION
       PERFORM 2003-PROCESOS
       PERFORM 2004-GENERAR-INFORME
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

       2002-INFORMACION.
           DISPLAY 'INGRESE SU NOMBRE: ' LINE 01 POSITION 01            19
           ACCEPT WS-NOMBRE-CLI          LINE 01 POSITION 20
           DISPLAY 'INGRESE SU CEDULA: ' LINE 02 POSITION 01            19
           ACCEPT WS-CEDULA-CLI          LINE 02 POSITION 20
           DISPLAY 'INGRESE SU FECHA DE NACIMIENTO DIA/MES/ANO: '       33
                                         LINE 03 POSITION 01
           ACCEPT  WS-FECHA-CLI          LINE 03 POSITION 45.

       2003-PROCESOS.
           ADD 2000 TO WS-ANO-SIS
           COMPUTE WS-EDAD = WS-ANO-SIS - WS-ANO-CLI.
           IF WS-MES-SIS < WS-MES-CLI
               COMPUTE WS-EDAD = WS-EDAD - 1
               IF WS-MES-SIS = WS-MES-CLI
                   IF WS-DIA-SIS < WS-DIA-CLI
                   COMPUTE WS-EDAD = WS-EDAD - 1
                   END-IF
               END-IF
           END-IF.

       2004-GENERAR-INFORME.
           DISPLAY CLEAR-SCREEN
           DISPLAY WS-ANO-SIS           LINE 01 POSITION 01
           DISPLAY WS-ANO-CLI           LINE 02 POSITION 01
           DISPLAY WS-MES-SIS           LINE 03 POSITION 01
           DISPLAY WS-MES-CLI           LINE 04 POSITION 01
           DISPLAY WS-DIA-SIS           LINE 05 POSITION 01
           DISPLAY WS-DIA-CLI           LINE 06 POSITION 01
           DISPLAY WS-EDAD              LINE 07 POSITION 01.

       3000-FINAL.
           STOP RUN.
