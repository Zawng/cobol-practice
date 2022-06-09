      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * ENTRADA:                                                          *
      * [X] CAPTURAR UN NOMBRE X(30)
      * [X] CAPTURAR FECHA NACIMIENTO 20-05-1925 DIA/MES/ANO
      * SALIDA:
      * [X] NOMBRE DIVIDIRLO EN 4 VARIABLES 
      * [X] CAMBIAR VOCALES POR NUMEROS
      * [X] MOSTRAR LOS DIAS EN SEPARADO
      ******************************************************************

      ******************************************************************
      *    IDENTIFICATION                                              *
      ******************************************************************
       ID DIVISION. 
       PROGRAM-ID.                         NO0C0006.
       AUTHOR.                             NOVATEC (EDWIN PAEZ).
       INSTALLATION.                       BBVA.
       DATE-WRITTEN.                       09-06-22.

      ******************************************************************
      *    ENVIRONMENT                                                 *
      ******************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    DATA                                                        *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
      ******************************************************************
      *    INFORMACION DEL USUARIO
      ******************************************************************
           02 WS-NOMBRE                    PIC X(30) VALUE SPACES.
           02 WS-INFORMACION.
              03 WS-PRIMER-NOM             PIC X(08) VALUE SPACES.
              03 WS-SEGUNDO-NOM            PIC X(08) VALUE SPACES.
              03 WS-PRIMER-APE             PIC X(08) VALUE SPACES.
              03 WS-SEGUNDO-APE            PIC X(08) VALUE SPACES.

           02 WS-ENTER                     PIC X(01) VALUE SPACES.
           02 WS-FECHA-NACI                PIC X(10) VALUE SPACES.
           02 WS-FECHA-NAC                 REDEFINES WS-FECHA-NACI.
              03 WS-DIA-NAC                PIC 9(02).
              03 FILLER                    PIC X(01).
              03 WS-MES-NAC                PIC 9(02).
              03 FILLER                    PIC X(01).
              03 WS-ANO-NAC                PIC 9(04).
           
      ******************************************************************
      *    FECHA Y HORA DEL SISTEMA                                    *
      ******************************************************************
           02 WS-FECHA-ACTUAL              PIC 9(06) VALUE ZEROES.
           02 WS-HORA-ACTUAL               PIC 9(08) VALUE ZEROES.
           02 WS-FECHA.                                                 10
              03 WS-DIA                    PIC 9(02) VALUE ZEROES.
              03 FILLER                    PIC X(01) VALUE '/'.
              03 WS-MES                    PIC 9(02) VALUE ZEROES.
              03 FILLER                    PIC X(01) VALUE '/'.
              03 WS-SIG                    PIC 9(02) VALUE 20.
              03 WS-ANO                    PIC 9(02) VALUE ZEROES.

           02 WS-HORA.                                                  8
              03 WS-HOR                    PIC 9(02) VALUE ZEROES.
              03 FILLER                    PIC X(01) VALUE ':'.
              03 WS-MIN                    PIC 9(02) VALUE ZEROES.
              03 FILLER                    PIC X(01) VALUE ':'.
              03 WS-SEG                    PIC 9(02) VALUE ZEROES.

      * PANTALLA PARA ELIMINAR INFORMACION DE CONSOLA "ERASE" 
       SCREEN SECTION.
       01  CLEAR-SCREEN BLANK SCREEN.

      ******************************************************************
      *    PROCEDURE                                                   *
      ******************************************************************
       PROCEDURE DIVISION.
      * PROCEDIMIENTO PRINCIPAL
       1000-PRINCIPAL.
           PERFORM 2000-OBTENER-FECHAS
           PERFORM 2003-INFORMACION-USUARIO
           PERFORM 2005-PROCESOS-VARIABLES
           PERFORM 2010-MOSTRAR-INFORMACION
           PERFORM 3000-FINAL.

      * OBTENEMOS LAS FECHAS DEL SISTEMA Y GENERAMOS LA FECHA CON
      * NUESTRO FORMATO DE DIA/MES/ANO 
       2000-OBTENER-FECHAS.
           ACCEPT WS-FECHA-ACTUAL          FROM DATE
           MOVE WS-FECHA-ACTUAL(5:2)       TO WS-DIA
           MOVE WS-FECHA-ACTUAL(3:2)       TO WS-MES
           MOVE WS-FECHA-ACTUAL(1:2)       TO WS-ANO
           ACCEPT WS-HORA-ACTUAL           FROM TIME
           MOVE WS-HORA-ACTUAL(1:2)        TO WS-HOR
           MOVE WS-HORA-ACTUAL(3:2)        TO WS-MIN
           MOVE WS-HORA-ACTUAL(5:2)        TO WS-SEG.

      * MUESTRA EN PRIMERA LINEA DE CONSOLA LA FECHA Y LA HORA
       2001-PANTALLA-FECHAS.
           DISPLAY 'FECHA DEL SISTEMA: '   LINE 01 POSITION 01
           DISPLAY WS-FECHA                LINE 01 POSITION 20          10
           DISPLAY 'HORA DEL SISTEMA: '    LINE 01 POSITION 54
           DISPLAY WS-HORA                 LINE 01 POSITION 72.         08

      * SOLICITAMOS LA INFORMACIÓN DEL USUARIO 
       2003-INFORMACION-USUARIO.
           PERFORM 2001-PANTALLA-FECHAS
           DISPLAY 'INGRESE SU NOMBRE COMPLETO: '                       27
                                           LINE 03 POSITION 01          
           ACCEPT WS-NOMBRE                LINE 03 POSITION 29
           DISPLAY 'INGRESE SU FECHA DE NACIMIENTO DIA/MES/ANO: '       45
                                           LINE 04 POSITION 01
           ACCEPT WS-FECHA-NACI            LINE 04 POSITION 45.

      * OPCIÓN PARA TERMINAR EL PROGRAMA AL PRESIONAR UNA TECLA "ENTER" 
       2004-SALIR.
           DISPLAY '<OPRIMA ENTER>'        LINE 24 POSITION 33          14
           ACCEPT WS-ENTER                 LINE 24 POSITION 48.

       2005-PROCESOS-VARIABLES.
           UNSTRING WS-NOMBRE             DELIMITED BY SPACE
                                          INTO WS-PRIMER-NOM
                                               WS-SEGUNDO-NOM
                                               WS-PRIMER-APE
                                               WS-SEGUNDO-APE
           END-UNSTRING.

      * MOSTRAR LA INFORMACIÓN PROCESADA
       2010-MOSTRAR-INFORMACION.
           DISPLAY CLEAR-SCREEN
           PERFORM 2001-PANTALLA-FECHAS
           DISPLAY 'PRIMER NOMBRE: '       LINE 03 POSITION 01          
           DISPLAY WS-PRIMER-NOM           LINE 03 POSITION 19
           DISPLAY 'SEGUNDO NOMBRE: '      LINE 04 POSITION 01          
           DISPLAY WS-SEGUNDO-NOM          LINE 04 POSITION 19
           DISPLAY 'PRIMER APELLIDO: '     LINE 05 POSITION 01          
           DISPLAY WS-PRIMER-APE           LINE 05 POSITION 19
           DISPLAY 'SEGUNDO APELLIDO: '    LINE 06 POSITION 01          17
           DISPLAY WS-SEGUNDO-APE          LINE 06 POSITION 19
           DISPLAY 'DIA NACIMIENTO: '      LINE 07 POSITION 01
           DISPLAY WS-DIA-NAC              LINE 07 POSITION 19
           DISPLAY 'MES NACIMIENTO: '      LINE 08 POSITION 01
           DISPLAY WS-MES-NAC              LINE 08 POSITION 19
           DISPLAY 'ANO NACIMIENTO: '      LINE 09 POSITION 01
           DISPLAY WS-ANO-NAC              LINE 09 POSITION 19
           PERFORM 2004-SALIR.

      * DETENER LA EJECUCIÓN DEL PROGRAMA 
       3000-FINAL.
           STOP RUN.
