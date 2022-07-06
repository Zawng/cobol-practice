
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
      * FECHA Y HORA DEL SISTEMA
      *----------------------------------------------------------------*
       COPY './RUTINAS/VARFECHAS.CPY'.

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
       01 WS-TOT-ENCUESTA            PIC 9(05) VALUE ZEROS. 

      * REQ-02 SEXO 
       01 WS-TOT-HOM                 PIC 9(05) VALUE ZEROS.
       01 WS-TOT-MUJ                 PIC 9(05) VALUE ZEROS.

      * REQ-03 EDADES
       01 WS-TOT-EDAD-JOV            PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EDAD-ADU            PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EDAD-MAY            PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EDAD-ANC            PIC 9(05) VALUE ZEROS.

      * REQ-04 ESTRATOS
       01 WS-TOT-EST-1               PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EST-2               PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EST-3               PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EST-4               PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EST-5               PIC 9(05) VALUE ZEROS.
       01 WS-TOT-EST-6               PIC 9(05) VALUE ZEROS.

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
       01  WS-TOT-SI-VAN-MAY-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ADU-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-SI-VAN-ANC-HOM     PIC 9(05) VALUE ZEROS.

       01  WS-TOT-NO-VAN-JOV-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-JOV-MUJ     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-MAY-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ADU-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-HOM     PIC 9(05) VALUE ZEROS.
       01  WS-TOT-NO-VAN-ANC-HOM     PIC 9(05) VALUE ZEROS.

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
       01 WS-TOT-SI-VAN-1-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-1-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-1-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-1-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-2-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-2-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-2-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-2-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-3-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-3-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-3-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-3-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-4-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-4-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-4-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-4-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-5-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-5-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-5-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-5-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-6-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-6-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-6-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-SI-VAN-6-ANC        PIC 9(05) VALUE ZEROS.

       01 WS-TOT-NO-VAN-1-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-1-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-1-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-1-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-2-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-2-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-2-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-2-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-3-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-3-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-3-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-3-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-4-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-4-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-4-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-4-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-5-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-5-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-5-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-5-ANC        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-6-JOV        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-6-MAY        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-6-ADU        PIC 9(05) VALUE ZEROS.
       01 WS-TOT-NO-VAN-6-ANC        PIC 9(05) VALUE ZEROS.
      *----------------------------------------------------------------*
      * UTILIDADES
      *----------------------------------------------------------------*
       01  WS-OPC                    PIC 9(02) VALUE ZEROS.

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
           PERFORM 1000-01-CONTESTAR-PREGUNTAS.
          *>  PERFORM 1000-02-TABULAR-ENCUESTA.
          
       1000-01-CONTESTAR-PREGUNTAS.
           PERFORM 1000-01-01-CAPTURA-SI-SEVA UNTIL WS-SI-SEVA OR 
                                                    WS-NO-SEVA
           PERFORM 1000-01-02-CAPTURA-EDAD    UNTIL WS-EDAD > 17
           PERFORM 1000-01-03-CAPTURA-SEXO    UNTIL WS-HOMBRE OR
                                                    WS-MUJER
           PERFORM 1000-01-04-CAPTURA-ESTRATO UNTIL WS-ESTRATO > 0 AND
                                                    < 7
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
