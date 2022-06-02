
      ******************************************************************
      * Author: EDWIN PAEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************

      *----------------------------------------------------------------*
      *                      IDENTIFICATION DIVISION                   *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                INDEXADOS.
       DATE-WRITTEN.              27-05-22.

      *----------------------------------------------------------------*
      *                        ENVIRONMENT DIVISION                    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Archivo f�sico en modo din�mico
       SELECT EMPLEADOS-ARCHIVO
       ASSIGN TO "../GENERADOS/EMPLEADOS.data"
       ORGANIZATION IS INDEXED
       RECORD KEY IS EMPLEADOS-ID
       ACCESS MODE IS DYNAMIC.

      *----------------------------------------------------------------*
      *                          DATA DIVISION                         *
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADOS-ARCHIVO.
       01  EMPLEADOS-REGISTRO.
           05 EMPLEADOS-ID        PIC 9(6).
           05 EMPLEADOS-NOMBRE    PIC X(25).
           05 EMPLEADOS-APELLIDOS PIC X(35).
           05 EMPLEADOS-EDAD      PIC 9(3).
           05 EMPLEADOS-TELEFONO  PIC X(9).
           05 EMPLEADOS-DIRECCION PIC X(35).

       WORKING-STORAGE SECTION.
       77  LEE-TODO               PIC X(1) VALUE "0".

      *----------------------------------------------------------------*
      *                         PROCEDURE DIVISION                     *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-ABRIR-ARCHIVO
       PERFORM 2004-LEE-SIGUIENTE-REGISTRO
       IF LEE-TODO = "1" THEN
           DISPLAY "No se encontraron registos en el archivo"
       ELSE
           PERFORM 2003-MUESTRA-CAMPOS UNTIL LEE-TODO = "1"
           PERFORM 2002-CERRAR-ARCHIVO.
       PERFORM 3000-FINAL.

       2000-PROCESOS.
       2001-ABRIR-ARCHIVO.
           OPEN I-O EMPLEADOS-ARCHIVO.

       2002-CERRAR-ARCHIVO.
           CLOSE EMPLEADOS-ARCHIVO.

       2003-MUESTRA-CAMPOS.
       DISPLAY
       "ID: "        EMPLEADOS-ID
       "NOMBRE: "    EMPLEADOS-NOMBRE
       "APELLIDO: "  EMPLEADOS-APELLIDOS
       "EDAD: "      EMPLEADOS-EDAD
       "TELEFONO: "  EMPLEADOS-TELEFONO
       "DIRECCION: " EMPLEADOS-TELEFONO.

       2004-LEE-SIGUIENTE-REGISTRO.
           READ EMPLEADOS-ARCHIVO NEXT RECORD AT END MOVE "1"
                                  TO LEE-TODO.

       3000-FINAL.
           STOP RUN.
           END PROGRAM INDEXADOS.
