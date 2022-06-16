
      ******************************************************************
      * AUTHOR: EDWIN PAEZ                                             *
      * PURPOSE: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * ENTRADA:
      * [X] HACER PROCESOS ARITMÉTICOS CON 2 NÚMEROS
      ******************************************************************

      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                             NO0C0008.
       AUTHOR.                                 NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                           BBVA.
       DATE-WRITTEN.                           14-JUN-22.

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
           02 WS-ENTER                         PIC X(01) VALUE SPACES.

      * NO GENERAR ERROR
           02 WS-NUMEROS.
             03 WS-NUM-01                      PIC S9(05).
             03 WS-NUM-02                      PIC S9(05).
             03 WS-TOT-SU                      PIC S9(06).
             03 WS-TOT-RE                      PIC S9(06).
             03 WS-TOT-MU                      PIC S9(10).
             03 WS-TOT-DI                      PIC S9(06)V9(02).
             03 WS-TOT-CO                      PIC S9(06)V9(02).
             03 WS-RES                         PIC S9(06)v9(02).

      * GENERAR ERROR 
          *>  02 WS-NUMEROS.
          *>    03 WS-NUM-01                      PIC S9(05).
          *>    03 WS-NUM-02                      PIC S9(05).
          *>    03 WS-TOT-SU                      PIC S9(01).
          *>    03 WS-TOT-RE                      PIC S9(01).
          *>    03 WS-TOT-MU                      PIC S9(01).
          *>    03 WS-TOT-DI                      PIC S9(01).
          *>    03 WS-TOT-CO                      PIC S9(01).

      *----------------------------------------------------------------*
      * MASCARAS
      *----------------------------------------------------------------*
           02 WS-MA-SU                         PIC +(07).
           02 WS-MA-RE                         PIC +(05)9.9(02).
           02 WS-MA-MU                         PIC +(10).
           02 WS-MA-DI                         PIC +(05)9.9(02).
           02 WS-MA-CO                         PIC +(09)9.9(02).

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
              03 WS-ANO-SIS                    PIC 9(02) VALUE ZEROES.

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
       PERFORM 2002-PANTALLA-FECHAS
       PERFORM 2003-INFORMACION
       PERFORM 2009-PANTALLA-SALIDA
       PERFORM 3000-FINAL.

      *----------------------------------------------------------------*
      * OBTENER LA FECHA Y LA HORA DEL SISTEMA
      *----------------------------------------------------------------*
       2001-FECHAS.
           ACCEPT WS-FECHA-ACT                 FROM DATE 
           MOVE WS-FECHA-ACT(5:2)              TO WS-DIA-SIS
           MOVE WS-FECHA-ACT(3:2)              TO WS-MES-SIS
           MOVE WS-FECHA-ACT(1:2)              TO WS-ANO-SIS
           ACCEPT WS-HORA-ACT                  FROM TIME
           MOVE WS-HORA-ACT(1:2)               TO WS-HOR-SIS
           MOVE WS-HORA-ACT(3:2)               TO WS-MIN-SIS
           MOVE WS-HORA-ACT(5:2)               TO WS-SEG-SIS.

       2002-PANTALLA-FECHAS.
           DISPLAY 'FEC SIS: '                 LINE 01 POSITION 01
           DISPLAY WS-FECHA-SIS                LINE 01 POSITION 10          
           DISPLAY 'HORA SIS: '                LINE 01 POSITION 62
           DISPLAY WS-HORA-SIS                 LINE 01 POSITION 72.         

       2003-INFORMACION.
           INITIALIZE WS-NUMEROS
           DISPLAY 'DIGITE DOS NUMEROS DE CINCO DIGITOS: '
                                               LINE 03 POSITION 22.
           DISPLAY '1) '                       LINE 05 POSITION 01
           ACCEPT  WS-NUM-01                   LINE 05 POSITION 04
           DISPLAY '2) '                       LINE 05 POSITION 10
           ACCEPT  WS-NUM-02                   LINE 05 POSITION 13.

       2004-SUMA.
      * SUMA EXITOSA
           ADD WS-NUM-01 WS-NUM-02 GIVING WS-TOT-SU ROUNDED
               ON SIZE ERROR
                   DISPLAY 'ERROR EN LA SUMA'  LINE 05 POSITION 01
               NOT ON SIZE ERROR
                   DISPLAY 'SUMA EXITOSA'      LINE 05 POSITION 01
           END-ADD
           MOVE WS-TOT-SU                      TO WS-MA-SU.

       2005-RESTA.
           SUBTRACT WS-NUM-01 FROM WS-NUM-02 GIVING WS-TOT-RE ROUNDED 
               ON SIZE ERROR
                   DISPLAY 'ERROR EN LA RESTA' LINE 09 POSITION 01
               NOT ON SIZE ERROR
                   DISPLAY 'RESTA EXITOSA'     LINE 09 POSITION 01
           END-SUBTRACT
           MOVE WS-TOT-RE                      TO WS-MA-RE.

       2006-MULTIPLICACION.
           MULTIPLY WS-NUM-01 BY WS-NUM-02 GIVING WS-TOT-MU ROUNDED 
               ON SIZE ERROR
                   DISPLAY 'ERROR EN LA MULTIPLICACION'
                                               LINE 13 POSITION 01
               NOT ON SIZE ERROR 
                   DISPLAY 'MULTIPLICACION EXITOSA'
                                               LINE 13 POSITION 01
           END-MULTIPLY
           MOVE WS-TOT-MU                      TO WS-MA-MU.

       2007-DIVISION.
           DIVIDE WS-NUM-01 BY WS-NUM-02 GIVING WS-TOT-DI ROUNDED 
                   REMAINDER WS-RES 
               ON SIZE ERROR 
                   DISPLAY 'ERROR EN LA DIVISION'
                                               LINE 17 POSITION 01
               NOT ON SIZE ERROR  
                   DISPLAY 'DIVISION EXITOSA'  LINE 17 POSITION 01
                   DISPLAY 'RESIDUO: '         LINE 17 POSITION 30
                   DISPLAY WS-RES              LINE 17 POSITION 40
           END-DIVIDE 
           MOVE WS-TOT-DI                      TO WS-MA-DI.

       2008-COMPUTADA.
           COMPUTE WS-TOT-CO ROUNDED = (WS-NUM-01 + WS-NUM-02) * (6 / 2)
               ON SIZE ERROR
                   DISPLAY 'ERROR EN LA COMPUTADA'
                                               LINE 21 POSITION 01
               NOT ON SIZE ERROR 
                   DISPLAY 'COMPUTADA EXITOSA' LINE 21 POSITION 01
           END-COMPUTE 
           MOVE WS-TOT-CO                      TO WS-MA-CO.

       2009-PANTALLA-SALIDA.
           DISPLAY CLEAR-SCREEN
           PERFORM 2002-PANTALLA-FECHAS
           DISPLAY 'NUMERO 1) '                LINE 02 POSITION 01
           DISPLAY WS-NUM-01                   LINE 02 POSITION 11
           DISPLAY 'NUMERO 2) '                LINE 02 POSITION 20
           DISPLAY WS-NUM-02                   LINE 02 POSITION 31
      * SUMA 
           PERFORM 2004-SUMA
           DISPLAY 'SUMA: '                    LINE 04 POSITION 01
           DISPLAY WS-MA-SU                    LINE 04 POSITION 07
      * RESTA
           PERFORM 2005-RESTA
           DISPLAY 'RESTA: '                   LINE 08 POSITION 01
           DISPLAY WS-MA-RE                    LINE 08 POSITION 08
      * MULTIPLICACION 
           PERFORM 2006-MULTIPLICACION
           DISPLAY 'MULTIPLICACION: '          LINE 12 POSITION 01
           DISPLAY WS-MA-MU                    LINE 12 POSITION 17
      * DIVISION 
           PERFORM 2007-DIVISION
           DISPLAY 'DIVISION: '                LINE 16 POSITION 01
           DISPLAY WS-MA-DI                    LINE 16 POSITION 11
      * COMPUTADA
           PERFORM 2008-COMPUTADA
           DISPLAY 'COMPUTADA: '               LINE 20 POSITION 01
           DISPLAY WS-MA-CO                    LINE 20 POSITION 12
           PERFORM 2010-SALIR.

       2010-SALIR.
           DISPLAY '<OPRIMA ENTER>'            LINE 24 POSITION 33      
           ACCEPT WS-ENTER                     LINE 24 POSITION 48.

       3000-FINAL.
           STOP RUN.
