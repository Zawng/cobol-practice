      * TODO: ¿POSIBLE MANERA DE EFECTUAR UNA OPCIÓN COMPUTADA?
      ******************************************************************
      * AUTHOR: EDWIN PÁEZ
      * DATE: 16-05-22
      * PURPOSE: PRACTICE COBOL
      ******************************************************************
      * SECCIÓN #1 - OBLIGATORIA
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PRACTICE.
       AUTHOR.                    EDWIN-PAEZ.
       INSTALLATION.              YOUTUBE.
       DATE-WRITTEN.              16/05/22.
       DATE-COMPILED.             16/05/22.
       REMARKS.                   CALCULADORA BÁSICA.

      * SECCIÓN #2
       ENVIRONMENT DIVISION.

      * SECCIÓN #3
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WSV-VARIABLES.
          05 WSV-NOMBRE           PIC X(11) VALUE 'EDWIN PÁEZ'.
          05 WSV-OPCION           PIC X VALUE LOW-VALUE .
          05 WSV-ARITMETICA.
             10 WSV-NUM-1         PIC 9(10) VALUE ZEROS.
             10 WSV-NUM-2         PIC 9(10) VALUE ZEROS.
             10 WSV-TOTAL         PIC Z(11) VALUE ZEROS.

       01 WSC-CONSTANTES.
          05 WSC-FILL             PIC X(80) VALUE ALL '-'.

          05 WSC-CABECERA.
            10 FILLER             PIC X(33) VALUE ALL ' '.
            10 WSC-TITULO         PIC X(13) VALUE ' CALCULADORA '.
            10 FILLER             PIC X(34) VALUE ALL ' '.

          05 WSC-OPCIONES.
             10 WSC-NOMBRE-S      PIC X(09) VALUE ' 1: SUMA '.
             10 WSC-NOMBRE-R      PIC X(10) VALUE ' 2: RESTA '.
             10 WSC-NOMBRE-M      PIC X(19) VALUE ' 3: MULTIPLICACION '.
             10 WSC-NOMBRE-D      PIC X(13) VALUE ' 4: DIVISION '.

      * SECCIÓN #4
       PROCEDURE DIVISION.
       010-MAIN.
           PERFORM 020-ITERATOR UNTIL WSV-OPCION EQUAL 0.
           DISPLAY "CALCULADORA HECHA POR: " QUOTE WSV-NOMBRE QUOTE
           STOP RUN.

       020-ITERATOR.
           PERFORM 030-HEADER.
           PERFORM 040-DATOS.
           PERFORM 050-VALIDACIONES.

       030-HEADER.
           DISPLAY WSC-FILL.
           DISPLAY WSC-CABECERA.
           DISPLAY WSC-FILL.

       040-DATOS.
           DISPLAY 'INGRESE LA OPCIÓN DE LA CALCULADORA:'
           DISPLAY WSC-NOMBRE-S.
           DISPLAY WSC-NOMBRE-R.
           DISPLAY WSC-NOMBRE-M.
           DISPLAY WSC-NOMBRE-D.
           DISPLAY " 5: SALIR DEL SISTEMA.".
           DISPLAY "OPCIÓN: "
           ACCEPT WSV-OPCION.

       050-VALIDACIONES.
           EVALUATE WSV-OPCION
               WHEN 0
                   DISPLAY 'APLICACIÓN TERMINADA'
               WHEN 1
                   PERFORM 070-INGRESAR
                   PERFORM 061-SUMA
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN 2
                   PERFORM 070-INGRESAR
                   PERFORM 062-RESTA
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN 3
                   PERFORM 070-INGRESAR
                   PERFORM 063-MULTIPLICACION
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN 4
                   PERFORM 070-INGRESAR
                   PERFORM 064-DIVISION
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN OTHER
                   DISPLAY "OPCIÓN NO ENCONTRADA, FINALIZANDO."
                   STOP RUN
           END-EVALUATE.

       060-ARITMETICA.
       061-SUMA.
           ADD WSV-NUM-1 TO WSV-NUM-2 GIVING WSV-TOTAL.
           DISPLAY "RESULTADO :" WSV-TOTAL.
       062-RESTA.
           SUBTRACT WSV-NUM-1 FROM WSV-NUM-2 GIVING WSV-TOTAL.
           DISPLAY "RESULTADO :" WSV-TOTAL.
       063-MULTIPLICACION.
           MULTIPLY WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL.
           DISPLAY "RESULTADO :" WSV-TOTAL.
       064-DIVISION.
           DIVIDE WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL.
           DISPLAY "RESULTADO :" WSV-TOTAL.

       070-INGRESAR.
           DISPLAY 'INGRESE LOS VALORES:'.
           DISPLAY 'NÚMERO 1:'.
           ACCEPT WSV-NUM-1.
           DISPLAY 'NÚMERO 2:'.
           ACCEPT WSV-NUM-2.