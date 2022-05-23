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

      * SECCIÓN #4
       PROCEDURE DIVISION.
       010-MAIN.
           PERFORM 015-ITERATOR UNTIL WSV-OPCION = 0.

       015-ITERATOR.
           PERFORM 020-HEADER.
           PERFORM 030-DATOS.
           PERFORM 040-VALIDACIONES.

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
           DISPLAY " 0: SALIR DEL SISTEMA.".
           DISPLAY "OPCIÓN: "
           ACCEPT WSV-OPCION.

       040-VALIDACIONES.
           EVALUATE WSV-OPCION
               WHEN 0
                   DISPLAY "APLICACIÓN TERMINADA"
                   PERFORM 0100-STOP
               WHEN 1
                   DISPLAY "OPCIÓN SELECCIONADA: SUMA"
                   PERFORM 070-INGRESAR
                   PERFORM 051-SUMA
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN 2
                   DISPLAY "OPCIÓN SELECCIONADA: RESTA"
                   PERFORM 070-INGRESAR
                   PERFORM 052-RESTA
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN 3
                   DISPLAY "OPCIÓN SELECCIONADA: MULTIPLICACION"
                   PERFORM 070-INGRESAR
                   PERFORM 053-MULTIPLICACION
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN 4
                   DISPLAY "OPCIÓN SELECCIONADA: DIVISION"
                   PERFORM 070-INGRESAR
                   PERFORM 054-DIVISION
                   CALL "C$SLEEP" USING 2 END-CALL
               WHEN OTHER
                   PERFORM 060-ERROR-OPCION
           END-EVALUATE.

       050-ARITMETICA.
       051-SUMA.
           ADD WSV-NUM-1 TO WSV-NUM-2 GIVING WSV-TOTAL.                 SUMAR
           DISPLAY "RESULTADO :" WSV-TOTAL.
       052-RESTA.
           SUBTRACT WSV-NUM-1 FROM WSV-NUM-2 GIVING WSV-TOTAL.          RESTA
           DISPLAY "RESULTADO :" WSV-TOTAL.
       053-MULTIPLICACION.
           MULTIPLY WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL.            MULTIPLICACIÓN
           DISPLAY "RESULTADO :" WSV-TOTAL.
       054-DIVISION.
           DIVIDE WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL.              DIVISIÓN
           DISPLAY "RESULTADO :" WSV-TOTAL.

       060-ERROR-OPCION.
           DISPLAY "OPCIÓN NO ENCONTRADA.".
           PERFORM 0100-STOP.

       070-INGRESAR.
           DISPLAY 'INGRESE LOS VALORES:'.
           DISPLAY 'NÚMERO 1:'.
           ACCEPT WSV-NUM-1.
           DISPLAY 'NÚMERO 2:'.
           ACCEPT WSV-NUM-2.

       0100-STOP.
           STOP RUN.
           END PROGRAM PRACTICE.