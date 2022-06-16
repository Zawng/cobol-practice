
      ******************************************************************
      * AUTHOR: EDWIN PAEZ                                             *
      * PURPOSE: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * ENTRADA:
      * [X] RECIBIR 5 NUMEROS DE 5 DIG
      * SALIDA:
      * [X] SUMA NUMEROS PARES:
      * [X] SUMA NUMEROS IMPARES:
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

           02 WS-NUMEROS.
             03 WS-NUM-01                      PIC 9(05).
             03 WS-NUM-02                      PIC 9(05).
             03 WS-NUM-03                      PIC 9(05).
             03 WS-NUM-04                      PIC 9(05).
             03 WS-NUM-05                      PIC 9(05).
             03 WS-TOT-01                      PIC 9(06).
             03 WS-TOT-02                      PIC 9(06).

      *----------------------------------------------------------------*
      * MASCARAS
      *----------------------------------------------------------------*
           02 WS-MP-01                         PIC Z(05).

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
       PERFORM 2004-PROCESOS
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
           DISPLAY 'DIGITE CINCO NUMEROS DE CINCO DIGITOS: '
                                               LINE 03 POSITION 20.
           DISPLAY '1) '                       LINE 05 POSITION 01
           ACCEPT  WS-NUM-01                   LINE 05 POSITION 04
           DISPLAY '2) '                       LINE 05 POSITION 10
           ACCEPT  WS-NUM-02                   LINE 05 POSITION 13
           DISPLAY '3) '                       LINE 05 POSITION 20
           ACCEPT  WS-NUM-03                   LINE 05 POSITION 23
           DISPLAY '4) '                       LINE 05 POSITION 30
           ACCEPT  WS-NUM-04                   LINE 05 POSITION 33
           DISPLAY '5) '                       LINE 05 POSITION 40
           ACCEPT  WS-NUM-05                   LINE 05 POSITION 43.

       2004-PROCESOS.
      * PARES 
           ADD WS-NUM-02 WS-NUM-04             GIVING WS-TOT-01 
           END-ADD 
           MOVE WS-TOT-01                      TO WS-MP-01
           DISPLAY 'SUMA DE LOS NUMEROS PARES: ' 
                                               LINE 07 POSITION 01
           DISPLAY WS-MP-01                    LINE 07 POSITION 29
      * IMPARES     
           ADD WS-NUM-01 WS-NUM-03 WS-NUM-05   GIVING WS-TOT-02
           END-ADD
           MOVE WS-TOT-02                      TO WS-MP-01
           DISPLAY 'SUMA DE LOS NUMEROS IMPARES: ' 
                                               LINE 07 POSITION 40
           DISPLAY WS-MP-01                    LINE 07 POSITION 71
           PERFORM 2005-SALIR.

       2005-SALIR.
           DISPLAY '<OPRIMA ENTER>'            LINE 24 POSITION 33      
           ACCEPT WS-ENTER                     LINE 24 POSITION 48.

       3000-FINAL.
           STOP RUN.
