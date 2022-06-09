      ******************************************************************
      * Author: EDWIN PAEZ                                             *
      * Purpose: PRACTICE COBOL                                        *
      * TODO:                                                          *
      * [X] GENERAR FECHA Y HORA DEL SISTEMA Y MOSTRARLAS EN LA LINEA 1*
      * [X] CAPTURAR CEDULA 9(15)                                      *
      * [X] CAPTURAR PRIMER NOMBRE X(15)                               *
      * [X] CAPTURAR SEGUNDO NOMBRE X(15)                              *
      * [X] CAPTURAR PRIMER APELLIDO X(15)                             *
      * [X] CAPTURAR SEGUNDO APELLIDO X(15)                            *
      * [X] MOSTRAR LA CÉDULA CENTRADA                                 *
      * [X] MOSTRAR LA UNIÓN DE LOS NOMBRES "NOMBRES APELLIDOS"        *
      * [X] MOSTRAR TOTAL CARACTERES NOMBRE                            *
      * [X] CONVERTIR LAS VOCALES DEL NOMBRE A NÚMEROS AEIOU = 12345   *
      * [X] CONVERTIR LOS NÚMEROS DE CEDULA A LETRAS                   *
      *               1234567890 = ABCDEFGHIJ                          *
      ******************************************************************

      ******************************************************************
      *    IDENTIFICATION                                              *
      ******************************************************************
       ID DIVISION. 
       PROGRAM-ID.                         NO0C0005.
       AUTHOR.                             NOVATEC (EDWIN PAEZ).
       INSTALLATION.                       BBVA.
       DATE-WRITTEN.                       07-06-22.

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
           02 WS-CEDULA                    PIC 9(15) VALUE ZEROES.
           02 WS-NOMBRE                    PIC X(15) VALUE SPACES.
           02 WS-NOMBRE-2                  PIC X(15) VALUE SPACES.
           02 WS-APELLIDO                  PIC X(15) VALUE SPACES.
           02 WS-APELLIDO-2                PIC X(15) VALUE SPACES.
           
      *    MASCARA PARA LA CEDULA     
           02 WS-CEDULA-MAS                PIC Z(15) VALUE ZEROES.
           02 WS-CEDULA-NUM                PIC Z(15) VALUE ZEROES.
           02 WS-CONTADOR                  PIC 9(02) VALUE 1.
           02 WS-NOMBRE-COM                PIC X(60) VALUE SPACES.
           02 WS-NOMBRE-NUM                PIC X(60) VALUE SPACES.
           02 WS-ENTER                     PIC X     VALUE SPACE.

           02 WS-ASTERISCOS                PIC X(80) VALUE ALL '*'.
           02 WS-TITULO.
              03 FILLER                    PIC X(27) VALUE ALL ' '.
              03 FILLER                    PIC X(26) VALUE
                                           'PROGRAMA DE PRACTICA COBOL'.
              03 FILLER                    PIC X(27) VALUE ALL ' '.

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
           DISPLAY WS-FECHA                LINE 01 POSITION 01
           DISPLAY WS-HORA                 LINE 01 POSITION 72.

       2002-SECCION-TITULOS.
           DISPLAY WS-ASTERISCOS           LINE 02 POSITION 01
           DISPLAY WS-TITULO               LINE 03 POSITION 01
           DISPLAY WS-ASTERISCOS           LINE 04 POSITION 01.

      * SOLICITAMOS LA INFORMACIÓN DEL USUARIO 
       2003-INFORMACION-USUARIO.
           PERFORM 2001-PANTALLA-FECHAS
           PERFORM 2002-SECCION-TITULOS
           DISPLAY 'INGRESE SU CEDULA: '   LINE 05 POSITION 01          19
           ACCEPT WS-CEDULA                LINE 05 POSITION 20
           DISPLAY 'INGRESE SU PRIMER NOMBRE: '                         25
                                           LINE 06 POSITION 01          
           ACCEPT WS-NOMBRE                LINE 06 POSITION 27
           DISPLAY 'INGRESE SU SEGUNDO NOMBRE: '                        26
                                           LINE 07 POSITION 01          
           ACCEPT WS-NOMBRE-2              LINE 07 POSITION 28
           DISPLAY 'INGRESE SU PRIMER APELLIDO: '                       27
                                           LINE 08 POSITION 01          
           ACCEPT WS-APELLIDO              LINE 08 POSITION 29
           DISPLAY 'INGRESE SU SEGUNDO APELLIDO: '                      29
                                           LINE 09 POSITION 01          
           ACCEPT WS-APELLIDO-2            LINE 09 POSITION 30.

      * OPCIÓN PARA TERMINAR EL PROGRAMA AL PRESIONAR UNA TECLA "ENTER" 
       2004-SALIR.
           DISPLAY '<OPRIMA ENTER>'        LINE 24 POSITION 33          14
           ACCEPT WS-ENTER                 LINE 24 POSITION 48.

       2005-PROCESOS-VARIABLES.
      * PASAR EL DATO DE LA CEDULA A UNA MASCARA 
           MOVE WS-CEDULA TO WS-CEDULA-MAS WS-CEDULA-NUM
      * UNIR LOS NOMBRES
           STRING WS-NOMBRE DELIMITED BY ' ' 
                  " " DELIMITED BY SIZE
                  WS-NOMBRE-2 DELIMITED BY ' '
                  " " DELIMITED BY SIZE
                  WS-APELLIDO DELIMITED BY ' '
                  " " DELIMITED BY SIZE
                  WS-APELLIDO-2 DELIMITED BY SPACE
                  INTO WS-NOMBRE-COM
      * CONTAR NUMERO DE CARACTERES DEL NOMBRE     
                  WITH POINTER WS-CONTADOR
                  NOT ON OVERFLOW COMPUTE WS-CONTADOR = WS-CONTADOR - 1
           END-STRING
      * REEMPLAZAR LOS CARACTERES DE LA CEDULA POR LETRAS     
           INSPECT WS-CEDULA-NUM CONVERTING '1234567890' TO 
                                            'ABCDEFGHIJ'
      * REEMPLAZAR LAS VOCALES POR NUMEROS
           MOVE WS-NOMBRE-COM TO WS-NOMBRE-NUM
           INSPECT WS-NOMBRE-NUM CONVERTING 'AEIOU' TO '12345'.

      * MOSTRAR LA INFORMACIÓN PROCESADA
       2010-MOSTRAR-INFORMACION.
           DISPLAY CLEAR-SCREEN
           PERFORM 2001-PANTALLA-FECHAS
           PERFORM 2002-SECCION-TITULOS
           DISPLAY 'CEDULA USUARIO: '      LINE 12 POSITION 01          15
           DISPLAY WS-CEDULA-MAS           LINE 12 POSITION 20
           DISPLAY 'NOMBRE COMPLETO: '     LINE 13 POSITION 01          17
           DISPLAY WS-NOMBRE-COM           LINE 13 POSITION 20
           DISPLAY 'TOTAL CARACTERES: '    LINE 14 POSITION 01          17
           DISPLAY WS-CONTADOR             LINE 14 POSITION 20
           DISPLAY 'CEDULA A LETRAS: '     LINE 15 POSITION 01          17
           DISPLAY WS-CEDULA-NUM           LINE 15 POSITION 20
           DISPLAY 'NOMBRE A NUMEROS: '    LINE 16 POSITION 01          18
           DISPLAY WS-NOMBRE-NUM           LINE 16 POSITION 20
           PERFORM 2004-SALIR.

      * DETENER LA EJECUCIÓN DEL PROGRAMA 
       3000-FINAL.
           STOP RUN.
