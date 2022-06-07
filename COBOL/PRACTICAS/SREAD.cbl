      ******************************************************************
      * Author: EDWIN PAEZ
      * Purpose: PRACTICE COBOL
      ******************************************************************

      *----------------------------------------------------------------*
      *                      IDENTIFICATION DIVISION                   *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                 READ-FILES.
       AUTHOR.                     EDWIN PAEZ.
       INSTALLATION.               NOVATEC.
       DATE-WRITTEN.               26-05-22.
       DATE-COMPILED.
       REMARKS.                    PROGRAMA QUE LEE UN ARCHIVO Y GENERA
                                   LA VISTA DE LOS REGISTROS.

      *----------------------------------------------------------------*
      *                        ENVIRONMENT DIVISION                    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            NOVATEC-IBM.
       OBJECT-COMPUTER.            NOVATEC-IBM.

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
           05 EMPLEADOS-ID         PIC X(6).
           05 EMPLEADOS-NOMBRE     PIC X(25).
           05 EMPLEADOS-APELLIDOS  PIC X(35).
           05 EMPLEADOS-EDAD       PIC X(3).
           05 EMPLEADOS-TELEFONO   PIC X(9).
           05 EMPLEADOS-DIRECCION  PIC X(35).

       WORKING-STORAGE SECTION.
       01  PRESENTACION.
           05 TEXTO-ID             PIC X(3) VALUE "ID:".
           05 MUESTRA-ID           PIC X(6).
           05 TEXTO-NOMBRE         PIC X(7) VALUE "Nombre:".
           05 MUESTRA-NOMBRE       PIC X(15).
           05 TEXTO-APELLIDOS      PIC X(10) VALUE "Apellidos:".
           05 MUESTRA-APELLIDOS    PIC X(20).
           05 TEXTO-EDAD           PIC X(5) VALUE "Edad:".
           05 MUESTRA-EDAD         PIC X(3).
           05 TEXTO-TELEFONO       PIC X(9) VALUE "Telefono:".
           05 MUESTRA-TELEFONO     PIC X(10).
           05 TEXTO-DIRECCION      PIC X(10) VALUE "Direccion:".
           05 MUESTRA-DIRECCION    PIC X(35).

       01  FIN-DEL-ARCHIVO         PIC X VALUE "1".
       01  MAXIMO-REGISTROS        PIC 99 VALUE ZEROES.
       01  GUARDA-ENTER            PIC X.

      *----------------------------------------------------------------*
      *                         PROCEDURE DIVISION                     *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       1000-PRINCIPAL.
       PERFORM 2001-APERTURA-ARCHIVO
       PERFORM 2005-LEE-SIGUIENTE-REGISTRO
       PERFORM 2003-MUESTRA-REGISTROS UNTIL FIN-DEL-ARCHIVO = "0"
       PERFORM 2002-CIERRE-ARCHIVO
       PERFORM 3000-FINAL.

       2000-PROCESOS.
       2001-APERTURA-ARCHIVO.
       OPEN INPUT EMPLEADOS-ARCHIVO.

       2002-CIERRE-ARCHIVO.
       CLOSE EMPLEADOS-ARCHIVO.

       2003-MUESTRA-REGISTROS.
       PERFORM 2004-MUESTRA-CAMPOS.
       PERFORM 2005-LEE-SIGUIENTE-REGISTRO.

       2004-MUESTRA-CAMPOS.
       IF MAXIMO-REGISTROS = 10
           PERFORM 2005-ENTER.
       MOVE EMPLEADOS-ID TO MUESTRA-ID
       MOVE EMPLEADOS-NOMBRE TO MUESTRA-NOMBRE
       MOVE EMPLEADOS-APELLIDOS TO MUESTRA-APELLIDOS
       MOVE EMPLEADOS-EDAD TO MUESTRA-EDAD
       MOVE EMPLEADOS-TELEFONO TO MUESTRA-TELEFONO
       MOVE EMPLEADOS-DIRECCION TO MUESTRA-DIRECCION
       DISPLAY PRESENTACION
       ADD 1 TO MAXIMO-REGISTROS.

       2005-LEE-SIGUIENTE-REGISTRO.
       READ EMPLEADOS-ARCHIVO NEXT RECORD AT END
                              MOVE "0" TO FIN-DEL-ARCHIVO.

       2005-ENTER.
       DISPLAY
       "Presione la tecla ENTER para ver la siguiente pagina...".
       ACCEPT GUARDA-ENTER.
       MOVE ZEROES TO MAXIMO-REGISTROS.

       3000-FINAL.
           STOP RUN.
           END PROGRAM READ-FILES.
