
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      ******************************************************************

      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                             NO0C0007.
       AUTHOR.                                 NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                           BBVA.
       DATE-WRITTEN.                           13-JUN-22.

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
           02 WS-ENTER   PIC X(01)       VALUE SPACES.
           02 WS-NUM-01  PIC 9(12)V9(2)  VALUE ZEROES.
           02 WS-NUM-02  PIC -9(12)V9(2) VALUE ZEROES.
      *----------------------------------------------------------------*
      * MASCARAS POSITIVOS
      *----------------------------------------------------------------*
           02 WS-MP-01   PIC 9(12).9(02).
           02 WS-MP-02   PIC 9(03),9(03),9(03),9(03).9(02).
           02 WS-MP-03   PIC Z(11)9.9(02).
           02 WS-MP-04   PIC *(11)9.9(02).
           02 WS-MP-05   PIC 9(03)/9(03)/9(03)/9(03).9(02).
           02 WS-MP-06   PIC *(11)9.9(02)CR.
           02 WS-MP-07   PIC +(12).9(02).

      *----------------------------------------------------------------*
      * MASCARAS NEGATIVOS
      *----------------------------------------------------------------*
           02 WS-MN-01   PIC $(11)9.9(02).
           02 WS-MN-02   PIC +(12).9(02).
           02 WS-MN-03   PIC /(11)9.9(02).
           02 WS-MN-04   PIC Z(12).9(02)DB.
           02 WS-MN-05   PIC *(03),*(03),*(03),*(03).9(02).
           02 WS-MN-06   PIC *(11)9.9(02)CR.
           02 WS-MN-07   PIC -(12).9(02).

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
       PERFORM 2002-INGRESAR-INFORMACION
       PERFORM 2005-MOSTRAR-M-POSITIVAS
       PERFORM 2006-MOSTRAR-M-NEGATIVAS
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

       2001-PANTALLA-FECHAS.
           DISPLAY 'FECHA DEL SISTEMA: '       LINE 01 POSITION 01
           DISPLAY WS-FECHA-SIS                LINE 01 POSITION 20
           DISPLAY 'HORA DEL SISTEMA: '        LINE 01 POSITION 54
           DISPLAY WS-HORA-SIS                 LINE 01 POSITION 72.

       2002-INGRESAR-INFORMACION.
           PERFORM 2001-PANTALLA-FECHAS
           DISPLAY 'PD: MAXIMO 12 DIGITOS ENTEROS Y 2 DIGITOS DECIMALES'
                                               LINE 03 POSITION 16
           DISPLAY 'NUMERO POSITIVO: ' 
                                               LINE 05 POSITION 01
           ACCEPT WS-NUM-01                    LINE 05 POSITION 18
           DISPLAY 'NUMERO NEGATIVO: '
                                               LINE 06 POSITION 01
           ACCEPT WS-NUM-02                    LINE 06 POSITION 18.

       2003-MASCARAS-POSITIVAS.
           MOVE WS-NUM-01 TO WS-MP-01 WS-MP-02 WS-MP-03 WS-MP-04
                WS-MP-05 WS-MP-06 WS-MP-07.

       2004-MASCARAS-NEGATIVAS.
           MOVE WS-NUM-02 TO WS-MN-01 WS-MN-02 WS-MN-03 WS-MN-04
                WS-MN-05 WS-MN-06 WS-MN-07.

       2005-MOSTRAR-M-POSITIVAS.
           DISPLAY CLEAR-SCREEN
           PERFORM 2001-PANTALLA-FECHAS
           PERFORM 2003-MASCARAS-POSITIVAS
           PERFORM 2004-MASCARAS-NEGATIVAS
           DISPLAY 'NUMERO POSITIVO :'         LINE 03 POSITION 01
           DISPLAY WS-NUM-01                   LINE 03 POSITION 19
           DISPLAY '1) 9(12).9(02) :'          LINE 04 POSITION 01
           DISPLAY WS-MP-01                    LINE 04 POSITION 38
           DISPLAY '2) 9(03),9(03),9(03),9(03).9(02) :'
                                               LINE 05 POSITION 01
           DISPLAY WS-MP-02                    LINE 05 POSITION 38
           DISPLAY '3) Z(11)9.9(02) :'         LINE 06 POSITION 01
           DISPLAY WS-MP-03                    LINE 06 POSITION 38
           DISPLAY '4) *(11)9.9(02) :'         LINE 07 POSITION 01
           DISPLAY WS-MP-04                    LINE 07 POSITION 38
           DISPLAY '5) 9(03)/9(03)/9(03)/9(03).9(02) :'
                                               LINE 08 POSITION 01
           DISPLAY WS-MP-05                    LINE 08 POSITION 38
           DISPLAY '6) *(11)9.9(02)CR :'       LINE 09 POSITION 01
           DISPLAY WS-MP-06                    LINE 09 POSITION 38
           DISPLAY '7) +(12).9(02 :'           LINE 10 POSITION 01
           DISPLAY WS-MP-07                    LINE 10 POSITION 38.

       2006-MOSTRAR-M-NEGATIVAS.
           DISPLAY 'NUMERO NEGATIVO :'         LINE 12 POSITION 01
           DISPLAY WS-NUM-02                   LINE 12 POSITION 19
           DISPLAY '1) *(11)9.9(02): '         LINE 13 POSITION 01
           DISPLAY WS-MN-01                    LINE 13 POSITION 38
           DISPLAY '2) +(12).9(02) :'          LINE 14 POSITION 01
           DISPLAY WS-MN-02                    LINE 14 POSITION 38
           DISPLAY '3) /(11)9.9(02) :'         LINE 15 POSITION 01
           DISPLAY WS-MN-03                    LINE 15 POSITION 38
           DISPLAY '4) Z(12).9(02)DB :'        LINE 16 POSITION 01
           DISPLAY WS-MN-04                    LINE 16 POSITION 38
           DISPLAY '5) *(03),*(03),*(03),*(03).9(02) :'
                                               LINE 17 POSITION 01
           DISPLAY WS-MN-05                    LINE 17 POSITION 38
           DISPLAY '6) *(11)9.9(02)CR :'       LINE 18 POSITION 01
           DISPLAY WS-MN-06                    LINE 18 POSITION 38
           DISPLAY '7) -(12).9(02) :'          LINE 19 POSITION 01
           DISPLAY WS-MN-07                    LINE 19 POSITION 38
           PERFORM 2007-ENTER.

       2007-ENTER.
           DISPLAY 'PRESIONE ENTER: '          LINE 24 POSITION 01
           ACCEPT WS-ENTER                     LINE 24 POSITION 18.

       3000-FINAL.
           STOP RUN.
