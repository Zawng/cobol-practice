
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
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

      *----------------------------------------------------------------*
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
           02 WS-FECHA-ACT               PIC 9(06) VALUE ZEROES.
           02 WS-HORA-ACT                PIC 9(08) VALUE ZEROES.
           02 WS-FECHA-SIS.
              03 WS-DIA-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE '/'.
              03 WS-MES-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE '/'.
              03 WS-SIG-SIS              PIC 9(02) VALUE 20.
              03 WS-ANO-SIS              PIC 9(02) VALUE ZEROES.

           02 WS-HORA-SIS.
              03 WS-HOR-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE ':'.
              03 WS-MIN-SIS              PIC 9(02) VALUE ZEROES.
              03 FILLER                  PIC X(01) VALUE ':'.
              03 WS-SEG-SIS              PIC 9(02) VALUE ZEROES.
       
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

       2003-INFORMACION.
           IF C1 AND (C3 OR C4) AND C2
               S1
               IF C3 OR C2
                   S2
               ELSE
                   S3
                   IF C1 AND (C4 OR C3) AND C1 OR C3
                       IF C3
                           CONTINUE
                       ELSE
                           IF C4 AND C3
                               S7
                           ELSE
                               S4
                               IF C3 OR C1 AND C4
                                   S5
                                   IF C1
                                       IF C4
                                           CONTINUE
                                       ELSE
                                           IF C2
                                               S6
           	                               ELSE
                                               CONTINUE
                                   ELSE
                                       CONTINUE	
                               ELSE
                                   CONTINUE
                   ELSE
                        CONTINUE
           ELSE
               S8.

       2004-OTRO.
           IF C1 AND (C3 OR C4) AND C2
             S1
             IF C3 OR C2
                 S2
             ELSE
                 S3
                 IF C1 AND (C4 OR C3) AND C1 OR C3
                     IF C3
                         CONTINUE
                     ELSE
                         IF C4 AND C3
                             S7
                         ELSE
                             S4
                             IF C3 OR C1 AND C4
                                 S5
                                 IF C1
                                     IF C4
                                         CONTINUE
                                     ELSE
                                         IF C2
                                             S6
                                         END-IF
                                     END-IF
                                 END-IF
                             END-IF
                         END-IF
                     END-IF
                 END-IF
             END-IF
           ELSE
               S8
           END-IF.

       2005-OTRO-IF.
           IF C1 AND (C3 OR C4) AND C2
             S1
             IF C3 OR C2
               S2
             ELSE 
               S3 
               IF C1 AND (C4 AND C3) AND C1 OR C3
                 IF C3
                   CONTINUE
                 ELSE 
                   IF C4 AND C3
                     S7
                   ELSE 
                     S4 
                     IF C3 OR C1 AND C4
                       S5
                       IF C1 AND C3
                         IF C4
                           CONTINUE 
                         ELSE
                           IF C2
                             S6
                           END-IF
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           ELSE
             S8
           END-IF.


       3000-FINAL.
           STOP RUN.
