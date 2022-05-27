      ******************************************************************
      * Author: EDWIN PAEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************
      *----------------------------------------------------------------*
      *                      IDENTIFICATION DIVISION                   *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                ARCHIVOS.
       AUTHOR.                    EDWIN-PAEZ.
       DATE-WRITTEN.              16-05-22.
       DATE-COMPILED.
       REMARKS.                   SISTEMA PARA AÑADIR USUARIOS.

      *----------------------------------------------------------------*
      *                        ENVIRONMENT DIVISION                    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL EMPLEADOS-ARCHIVO
              ASSIGN TO "../GENERADOS/EMPLEADOS.data"
              ORGANIZATION IS LINE SEQUENTIAL.

      *----------------------------------------------------------------*
      *                          DATA DIVISION                         *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADOS-ARCHIVO.
           01 EMPLEADOS-REGISTRO.
               05 EMPLEADOS-ID        PIC X(6).
               05 EMPLEADOS-NOMBRE    PIC X(25).
               05 EMPLEADOS-APELLIDOS PIC X(35).
               05 EMPLEADOS-EDAD      PIC X(3).
               05 EMPLEADOS-TELEFONO  PIC X(9).
               05 EMPLEADOS-DIRECCION PIC X(35).

       WORKING-STORAGE SECTION.
       01 SI-NO PIC X.
       01 ENTRADA PIC X.

      *----------------------------------------------------------------*
      *                         PROCEDURE DIVISION                     *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-ABRIR-ARCHIVO
       MOVE "S" TO SI-NO
       PERFORM 2003-AGREGAR-REGISTRO UNTIL SI-NO = "N"
       PERFORM 2002-CERRAR-ARCHIVO
       PERFORM 3000-FINAL.

       2000-PROCESOS.
       2001-ABRIR-ARCHIVO.
           OPEN EXTEND EMPLEADOS-ARCHIVO.

       2002-CERRAR-ARCHIVO.
           CLOSE EMPLEADOS-ARCHIVO.

       2003-AGREGAR-REGISTRO.
           MOVE "N" TO ENTRADA.
           PERFORM 2004-OBTENER-CAMPOS UNTIL ENTRADA = "S"
           PERFORM 2006-ESCRIBIR-REGISTRO
           PERFORM 2007-REINICIAR.

       2004-OBTENER-CAMPOS.
           MOVE SPACE TO EMPLEADOS-REGISTRO
           DISPLAY "Introduce un ID del nuevo empleado: ?".
           ACCEPT EMPLEADOS-ID
           DISPLAY "Introduce los nombres: ?".
           ACCEPT EMPLEADOS-NOMBRE
           DISPLAY "Introduce los apellidos: ?".
           ACCEPT EMPLEADOS-APELLIDOS
           DISPLAY "Introduce la edad: ?".
           ACCEPT EMPLEADOS-EDAD
           DISPLAY "Introduce un numero de telefono: ?".
           ACCEPT EMPLEADOS-TELEFONO
           DISPLAY "Introduce una direccion: ?".
           ACCEPT EMPLEADOS-DIRECCION
           PERFORM 2005-CONTINUAR.

       2005-CONTINUAR.
           MOVE "S" TO ENTRADA
           IF EMPLEADOS-NOMBRE = SPACE
           MOVE "N" TO ENTRADA.

       2006-ESCRIBIR-REGISTRO.
           WRITE EMPLEADOS-REGISTRO.

       2007-REINICIAR.
           DISPLAY "Desea almacenar otro registro? y/N"
           ACCEPT SI-NO
           IF SI-NO = "y" OR SI-NO = "Y"
               MOVE "S" TO SI-NO.
           IF SI-NO NOT = "y" OR SI-NO NOT = "Y"
               MOVE "N" TO SI-NO.

       3000-FINAL.
           STOP RUN.
           END PROGRAM ARCHIVOS.
