
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
       01  WS-TOT-EDA-EDU            PIC 9(05) VALUE ZEROS.
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

      * REQ-12 ESTRATO VS EDAD 
       01  WS-TOT-SI-VAN-1-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-1-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-1-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-1-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-2-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-3-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-4-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-5-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-6-ANC       PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-1-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-1-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-1-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-1-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-2-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-3-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-4-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-5-ANC       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6-JOV       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6-MAY       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6-ADU       PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-6-ANC       PIC 9(05) VALUE ZEROS.
      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPC                    PIC 9(02) VALUE ZEROS.
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
          *>  PERFORM 2000-RESULTADOS       UNTIL WS-OPC = 13
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
           PERFORM 1000-02-R11-TOT-SEVA-ESTR-SEXO
           PERFORM 1000-02-R12-TOT-SEVA-ESTR-EDAD.

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
               WHEN WS-ADULTOS ADD 1 TO WS-TOT-EDA-EDU
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

      * REQUERIMIENTO 12
       1000-02-R12-TOT-SEVA-ESTR-EDAD.
           EVALUATE TRUE
               WHEN WS-SI-SEVA
               EVALUATE TRUE
                 WHEN WS-ESTRA-1
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-SI-VAN-1-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-SI-VAN-1-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-SI-VAN-1-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-SI-VAN-1-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-2
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-SI-VAN-2-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-SI-VAN-2-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-SI-VAN-2-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-SI-VAN-2-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-3
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-SI-VAN-3-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-SI-VAN-3-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-SI-VAN-3-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-SI-VAN-3-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-4
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-SI-VAN-4-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-SI-VAN-4-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-SI-VAN-4-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-SI-VAN-4-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-5
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-SI-VAN-5-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-SI-VAN-5-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-SI-VAN-5-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-SI-VAN-5-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-6
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-SI-VAN-6-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-SI-VAN-6-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-SI-VAN-6-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-SI-VAN-6-ANC
                   END-EVALUATE
               END-EVALUATE
             WHEN WS-NO-SEVA
               EVALUATE TRUE
                 WHEN WS-ESTRA-1
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-NO-VAN-1-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-NO-VAN-1-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-NO-VAN-1-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-NO-VAN-1-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-2
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-NO-VAN-2-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-NO-VAN-2-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-NO-VAN-2-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-NO-VAN-2-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-3
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-NO-VAN-3-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-NO-VAN-3-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-NO-VAN-3-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-NO-VAN-3-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-4
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-NO-VAN-4-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-NO-VAN-4-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-NO-VAN-4-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-NO-VAN-4-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-5
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-NO-VAN-5-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-NO-VAN-5-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-NO-VAN-5-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-NO-VAN-5-ANC
                   END-EVALUATE
                 WHEN WS-ESTRA-6
                   EVALUATE TRUE
                    WHEN WS-JOVENES  ADD 1 TO WS-TOT-NO-VAN-6-JOV
                    WHEN WS-MAYORES  ADD 1 TO WS-TOT-NO-VAN-6-MAY
                    WHEN WS-ADULTOS  ADD 1 TO WS-TOT-NO-VAN-6-ADU
                    WHEN WS-ANCIANOS ADD 1 TO WS-TOT-NO-VAN-6-ANC
                   END-EVALUATE
               END-EVALUATE
           END-EVALUATE.

       999-MENSAJE-ERROR.
           STRING 'ERROR EN TAMANO EN LA VARIABLE : '
               WS-ERROR DELIMITED BY SIZE
               INTO WS-MENSAJE-ERROR
           END-STRING
           DISPLAY WS-MENSAJE-ERROR LINE 24 POSITION 05
           ACCEPT WS-ENTER          LINE 24 POSITION 67
           DISPLAY WS-BLANCOS       LINE 24 POSITION 01.
