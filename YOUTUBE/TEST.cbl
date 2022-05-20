      ******************************************************************
      * Author: Edwin Páez
      * Date: 20-05-22
      * Purpose: Practice COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.                                         Primera sección
       PROGRAM-ID.                      TEST.                              Título del programa
       AUTHOR.                          EDWIN-PAEZ.                        Autor del programa
       DATE-WRITTEN.                    16/05/22.                          Fecha de creación

       ENVIRONMENT DIVISION.                                            Segunda sección

       DATA DIVISION.                                                   Sección de datos
       WORKING-STORAGE SECTION.                                            Sección de variables
       01 WSV-VARIABLES.                                                   Variables
           05 WSV-NAME                  PIC X(05) VALUE 'EDWIN'.
       01 WSC-CONSTANTES.                                                  Constantes
           05 WSC-TITLE.
               010 FILLER               PIC X(33) VALUE ALL '*'.
               010 WSC-NAME             PIC X(13) VALUE ' CALCULADORA '.
               010 FILLER               PIC X(34) VALUE ALL '*'.
           05 WSC-ASTERISK.
               010 FILLER               PIC X(80) VALUE ALL '*'.

       PROCEDURE DIVISION.                                              Sección de procedimientos
       010-MAIN.
           PERFORM 020-HEADER.
           PERFORM 0100-STOP.

       020-HEADER.
           DISPLAY WSC-ASTERISK.
           DISPLAY WSC-TITLE.
           DISPLAY WSC-ASTERISK.

       0100-STOP.
           STOP RUN.
       END PROGRAM TEST.
