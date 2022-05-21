      * TODO: ERRORES EN LAS VALIDACIONES, SE EJECUTAN TODAS O SIGUEN
      ******************************************************************
      * Author: Edwin Páez
      * Date: 16-05-22
      * Purpose: Practice COBOL
      ******************************************************************
      * Sección #1 - Obligatoria
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PRACTICE.
       AUTHOR.                    EDWIN-PAEZ.
       INSTALLATION.              YOUTUBE.
       DATE-WRITTEN.              16/05/22.
       DATE-COMPILED.             16/05/22.
       REMARKS.                   CALCULADORA BÁSICA.

      * Sección #2
       ENVIRONMENT DIVISION.

      * Sección #3
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WSV-VARIABLES.
          05 WSV-NOMBRE           PIC X(11) VALUE 'EDWIN PÁEZ'.
          05 WSV-OPCION           PIC X VALUE ZEROS.
          05 WSV-ARITMETICA.
             10 WSV-NUM-1         PIC 9(2) VALUE ZEROS.
             10 WSV-NUM-2         PIC 9(2) VALUE ZEROS.
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
             10 WSC-NOMBRE-C      PIC X(14) VALUE ' 5: COMPUTADA '.

      * Sección #4
       PROCEDURE DIVISION.
       010-MAIN.
           PERFORM 020-HEADER.
           PERFORM 030-DATOS.
           PERFORM 040-VALIDACIONES.
           PERFORM 0100-STOP.

       020-HEADER.
           DISPLAY WSC-FILL.
           DISPLAY WSC-CABECERA.
           DISPLAY WSC-FILL.

       030-DATOS.
           DISPLAY 'INGRESE LA OPCIÓN DE LA CALCULADORA:'
           DISPLAY WSC-NOMBRE-S.
           DISPLAY WSC-NOMBRE-R.
           DISPLAY WSC-NOMBRE-M.
           DISPLAY WSC-NOMBRE-D.
           DISPLAY WSC-NOMBRE-C.
           DISPLAY "OPCIÓN: "
           ACCEPT WSV-OPCION.


       040-VALIDACIONES.
           IF WSV-OPCION > 5 OR < 0 THEN
             PERFORM 060-ERROR-OPCION
           ELSE
             IF WSV-OPCION EQUAL 1 THEN
                 DISPLAY "OPCIÓN SELECCIONADA: SUMA"
                 PERFORM 070-INGRESAR.
                 PERFORM 051-SUMA.
             IF WSV-OPCION EQUAL 2 THEN
                 DISPLAY "OPCIÓN SELECCIONADA: RESTA"
                 PERFORM 070-INGRESAR.
                 PERFORM 052-RESTA.
             IF WSV-OPCION EQUAL 3 THEN
                 DISPLAY "OPCIÓN SELECCIONADA: MULTIPLICACION"
                 PERFORM 070-INGRESAR.
                 PERFORM 053-MULTIPLICACION.
             IF WSV-OPCION EQUAL 4 THEN
                 DISPLAY "OPCIÓN SELECCIONADA: DIVISION"
                 PERFORM 070-INGRESAR.
                 PERFORM 054-DIVISION.
             IF WSV-OPCION EQUAL 5 THEN
                 DISPLAY "OPCIÓN SELECCIONADA: COMPUTADA"
                 PERFORM 070-INGRESAR.
                 PERFORM 055-COMPUTADA.

       050-ARITMETICA.
       051-SUMA.
           ADD WSV-NUM-1 TO WSV-NUM-2 GIVING WSV-TOTAL.                 Sumar
           DISPLAY "RESULTADO :" WSV-TOTAL.
       052-RESTA.
           SUBTRACT WSV-NUM-1 FROM WSV-NUM-2 GIVING WSV-TOTAL.          Resta
           DISPLAY "RESULTADO :" WSV-TOTAL.
       053-MULTIPLICACION.
           MULTIPLY WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL.            Multiplicación
           DISPLAY "RESULTADO :" WSV-TOTAL.
       054-DIVISION.
           DIVIDE WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL.              División
           DISPLAY "RESULTADO :" WSV-TOTAL.
       055-COMPUTADA.
           COMPUTE WSV-TOTAL = ((10 + 20) * 2) / 3.                     Compuesta
           DISPLAY "RESULTADO :" WSV-TOTAL.

       060-ERROR-OPCION.
           DISPLAY "OPCIÓN NO ENCONTRADA.".
           PERFORM 0100-STOP.

       070-INGRESAR.
           DISPLAY 'POR FAVOR, INGRESE LOS VALORES DE LOS NÚMEROS'.
           DISPLAY 'NÚMERO 1:'
           ACCEPT WSV-NUM-1.
           DISPLAY 'NÚMERO 2:'
           ACCEPT WSV-NUM-2.

       0100-STOP.
           STOP RUN.
           END PROGRAM PRACTICE.