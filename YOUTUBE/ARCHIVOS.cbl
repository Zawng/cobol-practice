      ******************************************************************
      * Author: EDWIN PÁEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                BASE.
       AUTHOR.                    EDWIN-PAEZ.
       DATE-WRITTEN.              16-05-22.
       ENVIRONMENT DIVISION.
      * ARCHIVO FISICO EN COBOL - INCLUYE EL NOMBRE DEL ARCHIVO EN EL
      * DISCO Y CÓMO SE ORGANIZARÁ EL ARCHIVO.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARCHIVO-EMPLEADOS ASSIGN TO "/PATH.ext"
            ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
      * ARCHIVO LÓGICO EN COBOL - INCLUYE EL DISEÑO DEL REGISTRO
       FILE SECTION.
       FD ARCHIVO-EMPLEADOS.                                            File descriptor - File definition, se conside un número especial
          01 EMPLEADOS.
            05 NOMBRE PIC X(25).
            05 APELLIDOS PIC X(50).
            05 TELEFONO PIC X(9).
            05 DIRECCION PIC X(50).
      * VARIABLES
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
        PERFORM 0100-STOP.
       0100-STOP.
           STOP RUN.
           END PROGRAM ARCHIVOS.