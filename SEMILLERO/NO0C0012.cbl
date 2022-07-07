
      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                       NO0C0012.
       AUTHOR.                           NOVATEC (EDWIN-PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     05-JUL-22.

      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * ENTRADAS
      *----------------------------------------------------------------*
       01  WS-SEVA                   PIC A VALUE SPACES.
           88 WS-SI-SEVA             VALUE 'S' 's'.
           88 WS-NO-SEVA             VALUE 'N' 'n'.
       01  WS-EDAD                   PIC 999 VALUE ZEROS.
           88 WS-JOVENES             VALUE 18 THRU 29.
           88 WS-ADULTOS             VALUE 30 THRU 45.
           88 WS-MAYORES             VALUE 46 THRU 65.
           88 WS-ANCIANOS            VALUE 66 THRU 999.
       01  WS-SEXO                   PIC A VALUE SPACES.
           88 WS-HOMBRE              VALUE 'H' 'h'.
           88 WS-MUJER               VALUE 'M' 'm'.
       01  WS-ESTRATO                PIC 9 VALUE ZEROS.
           88 WS-ESTRA-1             VALUE 1.
           88 WS-ESTRA-2             VALUE 2.
           88 WS-ESTRA-3             VALUE 3.
           88 WS-ESTRA-4             VALUE 4.
           88 WS-ESTRA-5             VALUE 5.
           88 WS-ESTRA-6             VALUE 6.
       01  WS-HAYMAS                 PIC A VALUE SPACES.
           88 WS-SI-HAY              VALUE 'S' 's'.
           88 WS-NO-HAY              VALUE 'N' 'n'.

       01  WS-ENTER                  PIC A VALUE SPACES.

      *----------------------------------------------------------------*
      * PROCESOS / SALIDAS
      *----------------------------------------------------------------*
      * REQ-01 TOTAL
       01  WS-TOT-ENCUESTA           PIC 9(05) VALUE ZEROS. 

      * REQ-02 SEXO 
       01  WS-TOT-HOM                PIC 9(05) VALUE ZEROS.
       01  WS-TOT-MUJ                PIC 9(05) VALUE ZEROS.

      * REQ-03 EDADES
       01  WS-TOT-EDA-JOV            PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EDA-ADU            PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EDA-MAY            PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EDA-ANC            PIC 9(05) VALUE ZEROS.

      * REQ-04 ESTRATOS
       01  WS-TOT-EST-1              PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EST-2              PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EST-3              PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EST-4              PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EST-5              PIC 9(05) VALUE ZEROS.
       01  WS-TOT-EST-6              PIC 9(05) VALUE ZEROS.

      * REQ-05 SE VAN
       01  WS-TOT-SI-SEVAN           PIC 9(05) VALUE ZEROS. 
       01  WS-TOT-NO-SEVAN           PIC 9(05) VALUE ZEROS. 

      * REQ-06 SI/NO RANGO DE EDAD 
       01  WS-TOT-SI-VAN-JOV         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC         PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-JOV         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC         PIC 9(05) VALUE ZEROS.

      * REQ-07 SI/NO SEXO 
       01  WS-TOT-SI-VAN-HOM         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MUJ         PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-HOM         PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MUJ         PIC 9(05) VALUE ZEROS.

      * REQ-08 SI/NO ESTRATO 
       01  WS-TOT-SI-VAN-1           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6           PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-1           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5           PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6           PIC 9(05) VALUE ZEROS.
      
      * REQ-09 EDAD SEXO
       01  WS-TOT-SI-VAN-JOV-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-JOV-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-MUJ     PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-JOV-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-MUJ     PIC 9(05) VALUE ZEROS.

      * REQ-10 EDAD ESTRATO 
       01  WS-TOT-SI-VAN-JOV-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-JOV-2       PIC 9(05) VALUE ZEROS. 
       01  WS-TOT-SI-VAN-JOV-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-JOV-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-JOV-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-JOV-6       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-MAY-6       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-6       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-6       PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-JOV-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-6       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-6       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-6       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-1       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-2       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-3       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-4       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-5       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-6       PIC 9(05) VALUE ZEROS.

      * REQ-11 ESTRATO VS SEXO
       01  WS-TOT-SI-VAN-1-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-1-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6-MUJ       PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-1-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-1-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5-MUJ       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6-HOM       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6-MUJ       PIC 9(05) VALUE ZEROS.

      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPC                    PIC 9(02) VALUE ZEROS.
           88 OPC-OK                 VALUE 1 THRU 12.
       01  WS-ERROR                  PIC X(30) VALUE SPACES.
       01  WS-MENSAJE-ERROR          PIC X(60) VALUE SPACES.
       01  WS-BLANCOS                PIC X(80) VALUE SPACES.

       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
           PERFORM 1000-CAPTURA-ENCUESTA      UNTIL WS-NO-HAY
           PERFORM 2000-RESULTADOS            UNTIL WS-OPC = 12
           STOP RUN.

       1000-CAPTURA-ENCUESTA.
           PERFORM 1000-01-CONTESTAR-PREGUNTAS
           PERFORM 1000-02-TABULAR-ENCUESTA.
          
       1000-01-CONTESTAR-PREGUNTAS.
           MOVE SPACES TO WS-SEVA
           PERFORM 1000-01-01-CAPTURA-SI-SEVA UNTIL WS-SI-SEVA OR 
                                                    WS-NO-SEVA
           MOVE ZEROS TO WS-EDAD
           PERFORM 1000-01-02-CAPTURA-EDAD    UNTIL WS-EDAD > 17
           MOVE SPACES TO WS-SEXO
           PERFORM 1000-01-03-CAPTURA-SEXO    UNTIL WS-HOMBRE OR
                                                    WS-MUJER
           MOVE ZEROS TO WS-ESTRATO
           PERFORM 1000-01-04-CAPTURA-ESTRATO UNTIL WS-ESTRATO > 0 AND
                                                    < 7
           MOVE SPACES TO WS-HAYMAS
           PERFORM 1000-01-05-CAPTURA-HAY-MAS UNTIL WS-SI-HAY OR 
                                                    WS-NO-HAY.

       1000-01-01-CAPTURA-SI-SEVA.
           DISPLAY CLEAR-SCREEN
           DISPLAY '1. PIENSA IRSE DE COLOMBIA? (S/N): '
                                         LINE 02 POSITION 02
           ACCEPT WS-SEVA                LINE 02 POSITION 36
           IF WS-SI-SEVA OR WS-NO-SEVA
               CONTINUE
           ELSE
               DISPLAY 'RESPUESTA ERRADA. DIGITE S o N'
                                         LINE 24 POSITION 30
               ACCEPT WS-ENTER           LINE 24 POSITION 70
           END-IF.

       1000-01-02-CAPTURA-EDAD.
           DISPLAY '2. QUE EDAD TIENE?:' LINE 04 POSITION 02
           ACCEPT WS-EDAD                LINE 04 POSITION 36
           IF WS-EDAD > 17
               CONTINUE
           ELSE
               DISPLAY 'RESPUESTA ERRADA. EDAD > 17'
                                         LINE 24 POSITION 30
           ACCEPT WS-ENTER               LINE 24 POSITION 70
           END-IF.

       1000-01-03-CAPTURA-SEXO.
           DISPLAY '3. DIGITE SEXO (H: HOMBRE/ M: MUJER)?:' 
                                         LINE 06 POSITION 02
           ACCEPT WS-SEXO                LINE 06 POSITION 50
           IF WS-HOMBRE OR WS-MUJER
               CONTINUE
           ELSE
               DISPLAY 'RESPUESTA ERRADA. DIGITE H o M'
                                         LINE 24 POSITION 30
           ACCEPT WS-ENTER               LINE 24 POSITION 70
           END-IF.

       1000-01-04-CAPTURA-ESTRATO.
           DISPLAY '4. DIGITE ESTRATO:'  LINE 08 POSITION 02
           ACCEPT WS-ESTRATO             LINE 08 POSITION 50
           IF WS-ESTRATO > 0 AND < 7
             CONTINUE
           ELSE
               DISPLAY 'RESPUESTA ERRADA. DIGITE ENTRE 1 Y 6'
                                         LINE 24 POSITION 30
           ACCEPT WS-ENTER               LINE 24 POSITION 70
           END-IF.

       1000-01-05-CAPTURA-HAY-MAS.
           DISPLAY '5. HAY MAS ENCUESTAS (S/N)?:'
                                         LINE 10 POSITION 02
           ACCEPT WS-HAYMAS              LINE 10 POSITION 50
           IF WS-SI-HAY OR WS-NO-HAY
             CONTINUE
           ELSE
             DISPLAY 'RESPUESTA ERRADA. DIGITE ENTRE S y N'
                                        LINE 24 POSITION 30
             ACCEPT WS-ENTER            LINE 24 POSITION 70
           END-IF.

       1000-02-TABULAR-ENCUESTA.
           PERFORM 1000-02-R01-TOT-ENCU
           PERFORM 1000-02-R02-TOT-SEXO
           PERFORM 1000-02-R03-TOT-EDAD
           PERFORM 1000-02-R04-TOT-ESTR
           PERFORM 1000-02-R05-TOT-SEVA
           PERFORM 1000-02-R06-TOT-SEVA-EDAD
           PERFORM 1000-02-R07-TOT-SEVA-SEXO
           PERFORM 1000-02-R08-TOT-SEVA-ESTR
           PERFORM 1000-02-R09-TOT-SEVA-EDAD-SEXO
           PERFORM 1000-02-R10-TOT-SEVA-EDAD-ESTR
           PERFORM 1000-02-R11-TOT-SEVA-ESTR-SEXO.

      * REQUERIMIENTO 1
       1000-02-R01-TOT-ENCU.
           ADD 1 TO WS-TOT-ENCUESTA 
               ON SIZE ERROR
                   MOVE 'WS-TOT-ENCUESTA' TO WS-ERROR
                   PERFORM 999-MENSAJE-ERROR
           END-ADD.

      * REQUERIMIENTO 2
       1000-02-R02-TOT-SEXO.
           EVALUATE TRUE
               WHEN WS-HOMBRE ADD 1 TO WS-TOT-HOM
               WHEN WS-MUJER ADD 1 TO WS-TOT-MUJ
           END-EVALUATE.

      * REQUERIMIENTO 3
       1000-02-R03-TOT-EDAD.
           EVALUATE TRUE
               WHEN WS-JOVENES ADD 1 TO WS-TOT-EDA-JOV
               WHEN WS-ADULTOS ADD 1 TO WS-TOT-EDA-ADU
               WHEN WS-MAYORES ADD 1 TO WS-TOT-EDA-MAY
               WHEN WS-ANCIANOS ADD 1 TO WS-TOT-EDA-ANC
           END-EVALUATE.

      * REQUERIMIENTO 4
       1000-02-R04-TOT-ESTR.
           EVALUATE TRUE
               WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-EST-1
               WHEN WS-ESTRA-2 ADD 1 TO WS-TOT-EST-2
               WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-EST-3
               WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-EST-4
               WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-EST-5
               WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-EST-6
           END-EVALUATE.

      * REQUERIMIENTO 5
       1000-02-R05-TOT-SEVA.
           EVALUATE TRUE
               WHEN WS-SI-SEVA ADD 1 TO WS-TOT-SI-SEVAN
               WHEN WS-NO-SEVA ADD 1 TO WS-TOT-NO-SEVAN
           END-EVALUATE.

      * REQUERIMIENTO 6
       1000-02-R06-TOT-SEVA-EDAD.
           EVALUATE TRUE
               WHEN WS-SI-SEVA
               EVALUATE TRUE
                 WHEN WS-JOVENES ADD 1 TO WS-TOT-SI-VAN-JOV
                 WHEN WS-ADULTOS ADD 1 TO  WS-TOT-SI-VAN-ADU
                 WHEN WS-MAYORES ADD 1 TO  WS-TOT-SI-VAN-MAY
                 WHEN WS-ANCIANOS ADD 1 TO  WS-TOT-SI-VAN-ANC
               END-EVALUATE
               WHEN WS-NO-SEVA
               EVALUATE TRUE
                 WHEN WS-JOVENES ADD 1 TO WS-TOT-NO-VAN-JOV
                 WHEN WS-ADULTOS ADD 1 TO  WS-TOT-NO-VAN-ADU
                 WHEN WS-MAYORES ADD 1 TO  WS-TOT-NO-VAN-MAY
                 WHEN WS-ANCIANOS ADD 1 TO  WS-TOT-NO-VAN-ANC
               END-EVALUATE
           END-EVALUATE.

      * REQUERIMIENTO 7
       1000-02-R07-TOT-SEVA-SEXO.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-SI-SEVA ALSO WS-HOMBRE 
                   ADD 1 TO WS-TOT-SI-VAN-HOM
               WHEN WS-SI-SEVA ALSO WS-MUJER
                   ADD 1 TO WS-TOT-SI-VAN-MUJ
               WHEN WS-NO-SEVA ALSO WS-HOMBRE
                   ADD 1 TO WS-TOT-NO-VAN-HOM
               WHEN WS-NO-SEVA ALSO WS-MUJER
                   ADD 1 TO WS-TOT-NO-VAN-MUJ
           END-EVALUATE.

      * REQUERIMIENTO 8
       1000-02-R08-TOT-SEVA-ESTR.
           EVALUATE TRUE ALSO TRUE
               WHEN WS-SI-SEVA ALSO WS-ESTRA-1
                   ADD 1 TO WS-TOT-SI-VAN-1
               WHEN WS-SI-SEVA ALSO WS-ESTRA-2
                   ADD 1 TO WS-TOT-SI-VAN-2
               WHEN WS-SI-SEVA ALSO WS-ESTRA-3
                   ADD 1 TO WS-TOT-SI-VAN-3
               WHEN WS-SI-SEVA ALSO WS-ESTRA-4
                   ADD 1 TO WS-TOT-SI-VAN-4
               WHEN WS-SI-SEVA ALSO WS-ESTRA-5
                   ADD 1 TO WS-TOT-SI-VAN-5
               WHEN WS-SI-SEVA ALSO WS-ESTRA-6
                   ADD 1 TO WS-TOT-SI-VAN-6
               WHEN WS-NO-SEVA ALSO WS-ESTRA-1
                   ADD 1 TO WS-TOT-NO-VAN-1
               WHEN WS-NO-SEVA ALSO WS-ESTRA-2
                   ADD 1 TO WS-TOT-NO-VAN-2
               WHEN WS-NO-SEVA ALSO WS-ESTRA-3
                   ADD 1 TO WS-TOT-NO-VAN-3
               WHEN WS-NO-SEVA ALSO WS-ESTRA-4
                   ADD 1 TO WS-TOT-NO-VAN-4
               WHEN WS-NO-SEVA ALSO WS-ESTRA-5
                   ADD 1 TO WS-TOT-NO-VAN-5
               WHEN WS-NO-SEVA ALSO WS-ESTRA-6
                   ADD 1 TO WS-TOT-NO-VAN-6
           END-EVALUATE.

      * REQUERIMIENTO 9
       1000-02-R09-TOT-SEVA-EDAD-SEXO.
           EVALUATE TRUE
             WHEN WS-SI-SEVA
               EVALUATE TRUE
                 WHEN WS-JOVENES
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-JOV-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-JOV-MUJ
                   END-EVALUATE
                 WHEN WS-ADULTOS
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-ADU-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-ADU-MUJ
                   END-EVALUATE
                 WHEN WS-MAYORES
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-MAY-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-MAY-MUJ
                   END-EVALUATE
                 WHEN WS-ANCIANOS
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-ANC-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-ANC-MUJ
                   END-EVALUATE
               END-EVALUATE
             WHEN WS-NO-SEVA
               EVALUATE TRUE
                 WHEN WS-JOVENES
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-JOV-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-JOV-MUJ
                   END-EVALUATE
                 WHEN WS-ADULTOS
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-ADU-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-ADU-MUJ
                   END-EVALUATE
                 WHEN WS-MAYORES
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-MAY-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-MAY-MUJ
                   END-EVALUATE
                 WHEN WS-ANCIANOS
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-ANC-HOM 
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-ANC-MUJ
                   END-EVALUATE
               END-EVALUATE
           END-EVALUATE.

      * REQUERIMIENTO 10
       1000-02-R10-TOT-SEVA-EDAD-ESTR.
           EVALUATE TRUE
             WHEN WS-SI-SEVA
               EVALUATE TRUE
                 WHEN WS-JOVENES
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-SI-VAN-JOV-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-SI-VAN-JOV-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-SI-VAN-JOV-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-SI-VAN-JOV-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-SI-VAN-JOV-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-SI-VAN-JOV-6
                   END-EVALUATE
                 WHEN WS-ADULTOS
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-SI-VAN-ADU-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-SI-VAN-ADU-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-SI-VAN-ADU-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-SI-VAN-ADU-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-SI-VAN-ADU-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-SI-VAN-ADU-6
                   END-EVALUATE
                 WHEN WS-MAYORES
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-SI-VAN-MAY-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-SI-VAN-MAY-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-SI-VAN-MAY-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-SI-VAN-MAY-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-SI-VAN-MAY-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-SI-VAN-MAY-6
                   END-EVALUATE
                 WHEN WS-ANCIANOS
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-SI-VAN-ANC-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-SI-VAN-ANC-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-SI-VAN-ANC-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-SI-VAN-ANC-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-SI-VAN-ANC-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-SI-VAN-ANC-6
                   END-EVALUATE
               END-EVALUATE
              WHEN WS-NO-SEVA
               EVALUATE TRUE
                 WHEN WS-JOVENES
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-NO-VAN-JOV-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-NO-VAN-JOV-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-NO-VAN-JOV-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-NO-VAN-JOV-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-NO-VAN-JOV-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-NO-VAN-JOV-6
                   END-EVALUATE
                 WHEN WS-ADULTOS
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-NO-VAN-ADU-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-NO-VAN-ADU-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-NO-VAN-ADU-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-NO-VAN-ADU-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-NO-VAN-ADU-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-NO-VAN-ADU-6
                   END-EVALUATE
                 WHEN WS-MAYORES
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-NO-VAN-MAY-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-NO-VAN-MAY-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-NO-VAN-MAY-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-NO-VAN-MAY-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-NO-VAN-MAY-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-NO-VAN-MAY-6
                   END-EVALUATE
                 WHEN WS-ANCIANOS
                   EVALUATE TRUE
                     WHEN WS-ESTRA-1 ADD 1 TO WS-TOT-NO-VAN-ANC-1
                     WHEN WS-ESTRA-2 ADD 1 to WS-TOT-NO-VAN-ANC-2
                     WHEN WS-ESTRA-3 ADD 1 TO WS-TOT-NO-VAN-ANC-3
                     WHEN WS-ESTRA-4 ADD 1 TO WS-TOT-NO-VAN-ANC-4
                     WHEN WS-ESTRA-5 ADD 1 TO WS-TOT-NO-VAN-ANC-5
                     WHEN WS-ESTRA-6 ADD 1 TO WS-TOT-NO-VAN-ANC-6
                   END-EVALUATE
               END-EVALUATE
           END-EVALUATE.

      * REQUERIMIENTO 11
       1000-02-R11-TOT-SEVA-ESTR-SEXO.
           EVALUATE TRUE
               WHEN WS-SI-SEVA
               EVALUATE TRUE
                 WHEN WS-ESTRA-1
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-1-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-1-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-2
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-2-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-2-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-3
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-3-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-3-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-4
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-4-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-4-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-5
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-5-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-5-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-6
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-SI-VAN-6-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-SI-VAN-6-MUJ
                   END-EVALUATE
               END-EVALUATE
             WHEN WS-NO-SEVA
               EVALUATE TRUE
                 WHEN WS-ESTRA-1
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-1-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-1-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-2
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-2-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-2-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-3
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-3-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-3-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-4
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-4-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-4-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-5
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-5-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-5-MUJ
                   END-EVALUATE
                 WHEN WS-ESTRA-6
                   EVALUATE TRUE
                     WHEN WS-HOMBRE ADD 1 TO WS-TOT-NO-VAN-6-HOM
                     WHEN WS-MUJER  ADD 1 TO WS-TOT-NO-VAN-6-MUJ
                   END-EVALUATE
               END-EVALUATE
           END-EVALUATE.


       2000-RESULTADOS.
           DISPLAY CLEAR-SCREEN
           DISPLAY 'MENU DE RESULTADOS'             LINE 02 POSITION 31
           DISPLAY '01. TOTAL ENCUESTADOS'          LINE 04 POSITION 10
           DISPLAY '02. POR SEXO'                   LINE 05 POSITION 10
           DISPLAY '03. POR EDAD'                   LINE 06 POSITION 10
           DISPLAY '04. POR ESTRATO'                LINE 07 POSITION 10
           DISPLAY '05. SI/NO SE VAN'               LINE 08 POSITION 10
           DISPLAY '06. SI/NO POR EDAD'             LINE 09 POSITION 10
           DISPLAY '07. SI/NO POR SEXO'             LINE 10 POSITION 10
           DISPLAY '08. SI/NO POR ESTRATO'          LINE 11 POSITION 10
           DISPLAY '09. SI/NO POR EDAD VS SEXO'     LINE 12 POSITION 10
           DISPLAY '10. SI/NO POR EDAD VS ESTRATO'  LINE 13 POSITION 10
           DISPLAY '11. SI/NO POR ESTRATO VS SEXO'  LINE 14 POSITION 10
           DISPLAY '12. SALIR'                      LINE 15 POSITION 10
           DISPLAY 'QUE OPCION DESEA?: '            LINE 16 POSITION 25
           MOVE ZEROS TO WS-OPC
           PERFORM UNTIL OPC-OK
               ACCEPT WS-OPC                        LINE 16 POSITION 45
               IF OPC-OK
                  CONTINUE
               ELSE
                   DISPLAY 'OPCION INVALIDA DIGITE ENTRE 1 Y 13'
                                                    LINE 24 POSITION 15
                   ACCEPT WS-ENTER                  LINE 24 POSITION 64
                   DISPLAY WS-BLANCOS               LINE 24 POSITION 01
               END-IF
           END-PERFORM

           DISPLAY CLEAR-SCREEN
           IF WS-OPC = 12
             CONTINUE
           ELSE
             DISPLAY 'RESULTADO DEL REQUERIMIENTO'    LINE 02 POSITION 26
           END-IF

           EVALUATE WS-OPC
             WHEN 01 PERFORM 2000-01-R01-TOT-ENCU
             WHEN 02 PERFORM 2000-02-R02-TOT-SEXO
             WHEN 03 PERFORM 2000-03-R03-TOT-EDAD
             WHEN 04 PERFORM 2000-04-R04-TOT-ESTR
             WHEN 05 PERFORM 2000-05-R05-TOT-SEVA
             WHEN 06 PERFORM 2000-06-R06-TOT-SEVA-EDAD
             WHEN 07 PERFORM 2000-07-R07-TOT-SEVA-SEXO
             WHEN 08 PERFORM 2000-08-R08-TOT-SEVA-ESTR
             WHEN 09 PERFORM 2000-09-R09-TOT-SEVA-EDAD-SEXO
             WHEN 10 PERFORM 2000-10-R10-TOT-SEVA-EDAD-ESTR
             WHEN 11 PERFORM 2000-11-R11-TOT-SEVA-ESTR-SEXO
           END-EVALUATE.

      * REQ-01 TOTAL ENCUENTADOS
       2000-01-R01-TOT-ENCU.
           DISPLAY 'TOTAL DE ENCUESTADOS:'  LINE 04 POSITION 05
           DISPLAY WS-TOT-ENCUESTA          LINE 04 POSITION 35
           PERFORM 999-ENTER.

      * REQ-02 TOTAL MUJERES Y HOMBRES ENCUENTADOS
       2000-02-R02-TOT-SEXO.
           DISPLAY 'TOTAL HOMBRES ENCUESTADOS:'  LINE 04 POSITION 05
           DISPLAY WS-TOT-HOM                    LINE 04 POSITION 35
           DISPLAY 'TOTAL MUJERES ENCUESTADOS:'  LINE 06 POSITION 05
           DISPLAY WS-TOT-MUJ                    LINE 06 POSITION 35
           PERFORM 999-ENTER.

      * REQ-03 TOTAL EDADES ENCUENTADOS
       2000-03-R03-TOT-EDAD.
           DISPLAY 'TOTAL JOVENES ENCUESTADOS:'      LINE 04 POSITION 05
           DISPLAY WS-TOT-EDA-JOV                    LINE 04 POSITION 35
           DISPLAY 'TOTAL MAYORES ENCUESTADOS:'      LINE 06 POSITION 05
           DISPLAY WS-TOT-EDA-MAY                    LINE 06 POSITION 35
           DISPLAY 'TOTAL ADULTOS ENCUESTADOS:'      LINE 08 POSITION 05
           DISPLAY WS-TOT-EDA-ADU                    LINE 08 POSITION 35
           DISPLAY 'TOTAL ANCIONOS ENCUESTADOS:'     LINE 10 POSITION 05
           DISPLAY WS-TOT-EDA-ANC                    LINE 10 POSITION 35
           PERFORM 999-ENTER.

      * REQ-04 TOTAL ESTRATOS ENCUENTADOS
       2000-04-R04-TOT-ESTR.
           DISPLAY 'TOTAL ESTRATO 1 ENCUESTADOS:'  LINE 04 POSITION 05
           DISPLAY WS-TOT-EST-1                    LINE 04 POSITION 35
           DISPLAY 'TOTAL ESTRATO 2 ENCUESTADOS:'  LINE 06 POSITION 05
           DISPLAY WS-TOT-EST-2                    LINE 06 POSITION 35
           DISPLAY 'TOTAL ESTRATO 3 ENCUESTADOS:'  LINE 08 POSITION 05
           DISPLAY WS-TOT-EST-3                    LINE 08 POSITION 35
           DISPLAY 'TOTAL ESTRATO 4 ENCUESTADOS:'  LINE 10 POSITION 05
           DISPLAY WS-TOT-EST-4                    LINE 10 POSITION 35
           DISPLAY 'TOTAL ESTRATO 5 ENCUESTADOS:'  LINE 12 POSITION 05
           DISPLAY WS-TOT-EST-5                    LINE 12 POSITION 35
           DISPLAY 'TOTAL ESTRATO 6 ENCUESTADOS:'  LINE 14 POSITION 05
           DISPLAY WS-TOT-EST-6                    LINE 14 POSITION 35
           PERFORM 999-ENTER.

      * REQ-05 TOTAL SI/NO SE VAN
       2000-05-R05-TOT-SEVA.
           DISPLAY 'TOTAL SE VAN:'     LINE 04 POSITION 05
           DISPLAY WS-TOT-SI-SEVAN     LINE 04 POSITION 35
           DISPLAY 'TOTAL NO SE VAN:'  LINE 06 POSITION 05
           DISPLAY WS-TOT-NO-SEVAN     LINE 06 POSITION 35
           PERFORM 999-ENTER.

      * REQ-06 TOTAL SE VAN POR EDAD
       2000-06-R06-TOT-SEVA-EDAD.
           DISPLAY 'TOTAL JOVENES SE VAN:'  LINE 04 POSITION 05
           DISPLAY WS-TOT-SI-VAN-JOV        LINE 04 POSITION 35
           DISPLAY 'TOTAL MAYORES SE VAN:'  LINE 06 POSITION 05
           DISPLAY WS-TOT-SI-VAN-MAY        LINE 06 POSITION 35
           DISPLAY 'TOTAL ADULTOS SE VAN:'  LINE 08 POSITION 05
           DISPLAY WS-TOT-SI-VAN-ADU        LINE 08 POSITION 35
           DISPLAY 'TOTAL ANCIANOS SE VAN:' LINE 10 POSITION 05
           DISPLAY WS-TOT-SI-VAN-ANC        LINE 10 POSITION 35

           DISPLAY 'TOTAL JOVENES NO SE VAN:'  LINE 12 POSITION 05
           DISPLAY WS-TOT-NO-VAN-JOV           LINE 12 POSITION 35
           DISPLAY 'TOTAL MAYORES NO SE VAN:'  LINE 14 POSITION 05
           DISPLAY WS-TOT-NO-VAN-MAY           LINE 14 POSITION 35
           DISPLAY 'TOTAL ADULTOS NO SE VAN:'  LINE 16 POSITION 05
           DISPLAY WS-TOT-NO-VAN-ADU           LINE 16 POSITION 35
           DISPLAY 'TOTAL ANCIANOS NO SE VAN:' LINE 18 POSITION 05
           DISPLAY WS-TOT-NO-VAN-ANC           LINE 18 POSITION 35
           PERFORM 999-ENTER.

      * REQ-07 TOTAL SE VAN POR SEXO 
       2000-07-R07-TOT-SEVA-SEXO.
           DISPLAY 'TOTAL HOMBRES SE VAN:'     LINE 04 POSITION 05
           DISPLAY WS-TOT-SI-VAN-HOM           LINE 04 POSITION 35
           DISPLAY 'TOTAL MUJERES SE VAN:'     LINE 06 POSITION 05
           DISPLAY WS-TOT-SI-VAN-MUJ           LINE 06 POSITION 35
           DISPLAY 'TOTAL HOMBRES NO SE VAN:'  LINE 08 POSITION 05
           DISPLAY WS-TOT-NO-VAN-HOM           LINE 08 POSITION 35
           DISPLAY 'TOTAL MUJERES NO SE VAN:'  LINE 10 POSITION 05
           DISPLAY WS-TOT-NO-VAN-MUJ           LINE 10 POSITION 35
           PERFORM 999-ENTER.

      * REQ-08 TOTAL SE VAN POR ESTRATO
       2000-08-R08-TOT-SEVA-ESTR.
           DISPLAY 'TOTAL ESTRATO 1 SE VAN:'     LINE 04 POSITION 05
           DISPLAY WS-TOT-SI-VAN-1               LINE 04 POSITION 35
           DISPLAY 'TOTAL ESTRATO 2 SE VAN:'     LINE 05 POSITION 05
           DISPLAY WS-TOT-SI-VAN-2               LINE 05 POSITION 35
           DISPLAY 'TOTAL ESTRATO 3 SE VAN:'     LINE 06 POSITION 05
           DISPLAY WS-TOT-SI-VAN-3               LINE 06 POSITION 35
           DISPLAY 'TOTAL ESTRATO 4 SE VAN:'     LINE 07 POSITION 05
           DISPLAY WS-TOT-SI-VAN-4               LINE 07 POSITION 35
           DISPLAY 'TOTAL ESTRATO 5 SE VAN:'     LINE 08 POSITION 05
           DISPLAY WS-TOT-SI-VAN-5               LINE 08 POSITION 35
           DISPLAY 'TOTAL ESTRATO 6 SE VAN:'     LINE 09 POSITION 05
           DISPLAY WS-TOT-SI-VAN-6               LINE 09 POSITION 35

           DISPLAY 'TOTAL ESTRATO 1 NO SE VAN:'  LINE 10 POSITION 05
           DISPLAY WS-TOT-NO-VAN-1               LINE 10 POSITION 35
           DISPLAY 'TOTAL ESTRATO 2 NO SE VAN:'  LINE 11 POSITION 05
           DISPLAY WS-TOT-NO-VAN-2               LINE 11 POSITION 35
           DISPLAY 'TOTAL ESTRATO 3 NO SE VAN:'  LINE 12 POSITION 05
           DISPLAY WS-TOT-NO-VAN-3               LINE 12 POSITION 35
           DISPLAY 'TOTAL ESTRATO 4 NO SE VAN:'  LINE 13 POSITION 05
           DISPLAY WS-TOT-NO-VAN-4               LINE 13 POSITION 35
           DISPLAY 'TOTAL ESTRATO 5 NO SE VAN:'  LINE 14 POSITION 05
           DISPLAY WS-TOT-NO-VAN-5               LINE 14 POSITION 35
           DISPLAY 'TOTAL ESTRATO 6 NO SE VAN:'  LINE 15 POSITION 05
           DISPLAY WS-TOT-NO-VAN-6               LINE 15 POSITION 35
           PERFORM 999-ENTER.

      * REQ-09 TOTAL SE VAN POR EDAD VS SEXO
       2000-09-R09-TOT-SEVA-EDAD-SEXO.
           DISPLAY 'LOS QUE SE VAN'          LINE 03 POSITION 15
                   'HOMBRES JOVENES:'        LINE 04 POSITION 05
                   'HOMBRES ADULTOS:'        LINE 06 POSITION 05
                   'HOMBRES MAYORES:'        LINE 08 POSITION 05
                   'HOMBRES ANCIANOS:'       LINE 10 POSITION 05

                   'MUJERES JOVENES:'        LINE 04 POSITION 40
                   'MUJERES ADULTOS:'        LINE 06 POSITION 40
                   'MUJERES MAYORES:'        LINE 08 POSITION 40
                   'MUJERES ANCIANOS:'       LINE 10 POSITION 40

                   'LOS QUE NO SE VAN'       LINE 11 POSITION 15
                   'HOMBRES JOVENES:'        LINE 12 POSITION 05
                   'HOMBRES ADULTOS:'        LINE 14 POSITION 05
                   'HOMBRES MAYORES:'        LINE 16 POSITION 05
                   'HOMBRES ANCIANOS:'       LINE 18 POSITION 05

                   'MUJERES JOVENES:'        LINE 12 POSITION 40
                   'MUJERES ADULTOS:'        LINE 14 POSITION 40
                   'MUJERES MAYORES:'        LINE 16 POSITION 40
                   'MUJERES ANCIANAS:'       LINE 18 POSITION 40

                   WS-TOT-SI-VAN-JOV-HOM     LINE 04 POSITION 25
                   WS-TOT-SI-VAN-JOV-MUJ     LINE 04 POSITION 65
                   WS-TOT-SI-VAN-ADU-HOM     LINE 06 POSITION 25
                   WS-TOT-SI-VAN-ADU-MUJ     LINE 06 POSITION 65
                   WS-TOT-SI-VAN-MAY-HOM     LINE 08 POSITION 25
                   WS-TOT-SI-VAN-MAY-MUJ     LINE 08 POSITION 65
                   WS-TOT-SI-VAN-ANC-HOM     LINE 10 POSITION 25
                   WS-TOT-SI-VAN-ANC-MUJ     LINE 10 POSITION 65

                   WS-TOT-NO-VAN-JOV-HOM     LINE 12 POSITION 25
                   WS-TOT-NO-VAN-JOV-MUJ     LINE 12 POSITION 65
                   WS-TOT-NO-VAN-ADU-HOM     LINE 14 POSITION 25
                   WS-TOT-NO-VAN-ADU-MUJ     LINE 14 POSITION 65
                   WS-TOT-NO-VAN-MAY-HOM     LINE 16 POSITION 25
                   WS-TOT-NO-VAN-MAY-MUJ     LINE 16 POSITION 65
                   WS-TOT-NO-VAN-ANC-HOM     LINE 18 POSITION 25
                   WS-TOT-NO-VAN-ANC-MUJ     LINE 18 POSITION 65
           PERFORM 999-ENTER.


      * REQ-10 TOTAL SE VAN POR  EDAD VS ESTRATO
       2000-10-R10-TOT-SEVA-EDAD-ESTR.
           DISPLAY 'LOS QUE SE VAN'           LINE 03 POSITION 01
                   'JOVENES'                  LINE 04 POSITION 01
                   'ESTRATO 1:'               LINE 05 POSITION 05
                   'ESTRATO 2:'               LINE 06 POSITION 05
                   'ESTRATO 3:'               LINE 07 POSITION 05
                   'ESTRATO 4:'               LINE 08 POSITION 05
                   'ESTRATO 5:'               LINE 09 POSITION 05
                   'ESTRATO 6:'               LINE 10 POSITION 05
                   WS-TOT-SI-VAN-JOV-1        LINE 05 POSITION 16 
                   WS-TOT-SI-VAN-JOV-2        LINE 06 POSITION 16 
                   WS-TOT-SI-VAN-JOV-3        LINE 07 POSITION 16 
                   WS-TOT-SI-VAN-JOV-4        LINE 08 POSITION 16 
                   WS-TOT-SI-VAN-JOV-5        LINE 09 POSITION 16 
                   WS-TOT-SI-VAN-JOV-6        LINE 10 POSITION 16 

                   'MAYORES'                  LINE 04 POSITION 50
                   'ESTRATO 1:'               LINE 05 POSITION 50
                   'ESTRATO 2:'               LINE 06 POSITION 50
                   'ESTRATO 3:'               LINE 07 POSITION 50
                   'ESTRATO 4:'               LINE 08 POSITION 50
                   'ESTRATO 5:'               LINE 09 POSITION 50
                   'ESTRATO 6:'               LINE 10 POSITION 50
                   WS-TOT-SI-VAN-MAY-1        LINE 05 POSITION 62 
                   WS-TOT-SI-VAN-MAY-2        LINE 06 POSITION 62 
                   WS-TOT-SI-VAN-MAY-3        LINE 07 POSITION 62 
                   WS-TOT-SI-VAN-MAY-4        LINE 08 POSITION 62 
                   WS-TOT-SI-VAN-MAY-5        LINE 09 POSITION 62 
                   WS-TOT-SI-VAN-MAY-6        LINE 10 POSITION 62 

                   'ADULTOS'                  LINE 12 POSITION 01
                   'ESTRATO 1:'               LINE 13 POSITION 05
                   'ESTRATO 2:'               LINE 14 POSITION 05
                   'ESTRATO 3:'               LINE 15 POSITION 05
                   'ESTRATO 4:'               LINE 16 POSITION 05
                   'ESTRATO 5:'               LINE 17 POSITION 05
                   'ESTRATO 6:'               LINE 18 POSITION 05
                   WS-TOT-SI-VAN-ADU-1        LINE 13 POSITION 16 
                   WS-TOT-SI-VAN-ADU-2        LINE 14 POSITION 16 
                   WS-TOT-SI-VAN-ADU-3        LINE 15 POSITION 16 
                   WS-TOT-SI-VAN-ADU-4        LINE 16 POSITION 16 
                   WS-TOT-SI-VAN-ADU-5        LINE 17 POSITION 16 
                   WS-TOT-SI-VAN-ADU-6        LINE 18 POSITION 16 

                   'ANCIANOS'                 LINE 12 POSITION 50
                   'ESTRATO 1:'               LINE 13 POSITION 50
                   'ESTRATO 2:'               LINE 14 POSITION 50
                   'ESTRATO 3:'               LINE 15 POSITION 50
                   'ESTRATO 4:'               LINE 16 POSITION 50
                   'ESTRATO 5:'               LINE 17 POSITION 50
                   'ESTRATO 6:'               LINE 18 POSITION 50
                   WS-TOT-SI-VAN-MAY-1        LINE 13 POSITION 62 
                   WS-TOT-SI-VAN-MAY-2        LINE 14 POSITION 62 
                   WS-TOT-SI-VAN-MAY-3        LINE 15 POSITION 62 
                   WS-TOT-SI-VAN-MAY-4        LINE 16 POSITION 62 
                   WS-TOT-SI-VAN-MAY-5        LINE 17 POSITION 62 
                   WS-TOT-SI-VAN-MAY-6        LINE 18 POSITION 62 
           PERFORM 999-ENTER
            DISPLAY 'LOS QUE NO SE VAN'       LINE 03 POSITION 01
                   'JOVENES'                  LINE 04 POSITION 01
                   'ESTRATO 1:'               LINE 05 POSITION 05
                   'ESTRATO 2:'               LINE 06 POSITION 05
                   'ESTRATO 3:'               LINE 07 POSITION 05
                   'ESTRATO 4:'               LINE 08 POSITION 05
                   'ESTRATO 5:'               LINE 09 POSITION 05
                   'ESTRATO 6:'               LINE 10 POSITION 05
                   WS-TOT-NO-VAN-JOV-1        LINE 05 POSITION 16 
                   WS-TOT-NO-VAN-JOV-2        LINE 06 POSITION 16 
                   WS-TOT-NO-VAN-JOV-3        LINE 07 POSITION 16 
                   WS-TOT-NO-VAN-JOV-4        LINE 08 POSITION 16 
                   WS-TOT-NO-VAN-JOV-5        LINE 09 POSITION 16 
                   WS-TOT-NO-VAN-JOV-6        LINE 10 POSITION 16 

                   'MAYORES'                  LINE 04 POSITION 50
                   'ESTRATO 1:'               LINE 05 POSITION 50
                   'ESTRATO 2:'               LINE 06 POSITION 50
                   'ESTRATO 3:'               LINE 07 POSITION 50
                   'ESTRATO 4:'               LINE 08 POSITION 50
                   'ESTRATO 5:'               LINE 09 POSITION 50
                   'ESTRATO 6:'               LINE 10 POSITION 50
                   WS-TOT-NO-VAN-MAY-1        LINE 05 POSITION 62 
                   WS-TOT-NO-VAN-MAY-2        LINE 06 POSITION 62 
                   WS-TOT-NO-VAN-MAY-3        LINE 07 POSITION 62 
                   WS-TOT-NO-VAN-MAY-4        LINE 08 POSITION 62 
                   WS-TOT-NO-VAN-MAY-5        LINE 09 POSITION 62 
                   WS-TOT-NO-VAN-MAY-6        LINE 10 POSITION 62 

                   'ADULTOS'                  LINE 12 POSITION 01
                   'ESTRATO 1:'               LINE 13 POSITION 05
                   'ESTRATO 2:'               LINE 14 POSITION 05
                   'ESTRATO 3:'               LINE 15 POSITION 05
                   'ESTRATO 4:'               LINE 16 POSITION 05
                   'ESTRATO 5:'               LINE 17 POSITION 05
                   'ESTRATO 6:'               LINE 18 POSITION 05
                   WS-TOT-NO-VAN-ADU-1        LINE 13 POSITION 16 
                   WS-TOT-NO-VAN-ADU-2        LINE 14 POSITION 16 
                   WS-TOT-NO-VAN-ADU-3        LINE 15 POSITION 16 
                   WS-TOT-NO-VAN-ADU-4        LINE 16 POSITION 16 
                   WS-TOT-NO-VAN-ADU-5        LINE 17 POSITION 16 
                   WS-TOT-NO-VAN-ADU-6        LINE 18 POSITION 16 

                   'ANCIANOS'                 LINE 12 POSITION 50
                   'ESTRATO 1:'               LINE 13 POSITION 50
                   'ESTRATO 2:'               LINE 14 POSITION 50
                   'ESTRATO 3:'               LINE 15 POSITION 50
                   'ESTRATO 4:'               LINE 16 POSITION 50
                   'ESTRATO 5:'               LINE 17 POSITION 50
                   'ESTRATO 6:'               LINE 18 POSITION 50
                   WS-TOT-NO-VAN-MAY-1        LINE 13 POSITION 62 
                   WS-TOT-NO-VAN-MAY-2        LINE 14 POSITION 62 
                   WS-TOT-NO-VAN-MAY-3        LINE 15 POSITION 62 
                   WS-TOT-NO-VAN-MAY-4        LINE 16 POSITION 62 
                   WS-TOT-NO-VAN-MAY-5        LINE 17 POSITION 62 
                   WS-TOT-NO-VAN-MAY-6        LINE 18 POSITION 62 
           PERFORM 999-ENTER.


      * REQ-11 TOTAL SE VAN POR ESTRATO VS SEXO
       2000-11-R11-TOT-SEVA-ESTR-SEXO.
           DISPLAY 'LOS QUE SE VAN'           LINE 03 POSITION 01
                   'HOMBRES'                  LINE 04 POSITION 01
                   'ESTRATO 1:'               LINE 05 POSITION 05
                   'ESTRATO 2:'               LINE 06 POSITION 05
                   'ESTRATO 3:'               LINE 07 POSITION 05
                   'ESTRATO 4:'               LINE 08 POSITION 05
                   'ESTRATO 5:'               LINE 09 POSITION 05
                   'ESTRATO 6:'               LINE 10 POSITION 05
                   WS-TOT-SI-VAN-1-HOM        LINE 05 POSITION 16 
                   WS-TOT-SI-VAN-2-HOM        LINE 06 POSITION 16 
                   WS-TOT-SI-VAN-3-HOM        LINE 07 POSITION 16 
                   WS-TOT-SI-VAN-4-HOM        LINE 08 POSITION 16 
                   WS-TOT-SI-VAN-5-HOM        LINE 09 POSITION 16 
                   WS-TOT-SI-VAN-6-HOM        LINE 10 POSITION 16

                   'MUJERES'                  LINE 04 POSITION 50
                   'ESTRATO 1:'               LINE 05 POSITION 50
                   'ESTRATO 2:'               LINE 06 POSITION 50
                   'ESTRATO 3:'               LINE 07 POSITION 50
                   'ESTRATO 4:'               LINE 08 POSITION 50
                   'ESTRATO 5:'               LINE 09 POSITION 50
                   'ESTRATO 6:'               LINE 10 POSITION 50
                   WS-TOT-SI-VAN-1-MUJ        LINE 05 POSITION 62 
                   WS-TOT-SI-VAN-2-MUJ        LINE 06 POSITION 62 
                   WS-TOT-SI-VAN-3-MUJ        LINE 07 POSITION 62 
                   WS-TOT-SI-VAN-4-MUJ        LINE 08 POSITION 62 
                   WS-TOT-SI-VAN-5-MUJ        LINE 09 POSITION 62 
                   WS-TOT-SI-VAN-6-MUJ        LINE 10 POSITION 62


                   'LOS QUE NO SE VAN'        LINE 11 POSITION 01
                   'HOMBRES'                  LINE 12 POSITION 01
                   'ESTRATO 1:'               LINE 13 POSITION 05
                   'ESTRATO 2:'               LINE 14 POSITION 05
                   'ESTRATO 3:'               LINE 15 POSITION 05
                   'ESTRATO 4:'               LINE 16 POSITION 05
                   'ESTRATO 5:'               LINE 17 POSITION 05
                   'ESTRATO 6:'               LINE 18 POSITION 05
                   WS-TOT-NO-VAN-1-HOM        LINE 13 POSITION 16 
                   WS-TOT-NO-VAN-2-HOM        LINE 14 POSITION 16 
                   WS-TOT-NO-VAN-3-HOM        LINE 15 POSITION 16 
                   WS-TOT-NO-VAN-4-HOM        LINE 16 POSITION 16 
                   WS-TOT-NO-VAN-5-HOM        LINE 17 POSITION 16 
                   WS-TOT-NO-VAN-6-HOM        LINE 18 POSITION 16 

                   'MUJERES'                  LINE 12 POSITION 50
                   'ESTRATO 1:'               LINE 13 POSITION 50
                   'ESTRATO 2:'               LINE 14 POSITION 50
                   'ESTRATO 3:'               LINE 15 POSITION 50
                   'ESTRATO 4:'               LINE 16 POSITION 50
                   'ESTRATO 5:'               LINE 17 POSITION 50
                   'ESTRATO 6:'               LINE 18 POSITION 50

                   WS-TOT-NO-VAN-1-MUJ        LINE 13 POSITION 62 
                   WS-TOT-NO-VAN-2-MUJ        LINE 14 POSITION 62 
                   WS-TOT-NO-VAN-3-MUJ        LINE 15 POSITION 62 
                   WS-TOT-NO-VAN-4-MUJ        LINE 16 POSITION 62 
                   WS-TOT-NO-VAN-5-MUJ        LINE 17 POSITION 62 
                   WS-TOT-NO-VAN-6-MUJ        LINE 18 POSITION 62
           PERFORM 999-ENTER.

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
