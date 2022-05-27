      ******************************************************************
      * Author: Edwin Páez
      * Date: 16-05-22
      * Purpose: Practice COBOL
      ******************************************************************
      * Sección #1 - Obligatoria
      IDENTIFICATION DIVISION.

      * Sección #2
       ENVIRONMENT DIVISION.                                            Se usa para describir la estructura física de archivos del programa

       CONFIGURATION SECTION.                                           Primera parte de la environment
       SOURCE-COMPUTER.                 PC-NOVATEC.                        PC donde se escribió el código
       OBJECT-COMPUTER.                 PC-NOVATEC.                        PC donde se ejecutará el código
       SPECIAL-NAMES.                                                      Cambiar valores de las constantes
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.                                            Segunda parte de la environment
       FILE-CONTROL.                                                    Sirve para especificar nombres de archivos etc
       SELECT [OPTIONAL] NOMBRE-ARCHIVO
       ASSIGN TO TIPO-DE-DISPOSITIVO.
       ORGANIZATION IS TIPO-DE-ORGANIZACION.
       ACCESS MODE IS MODO-ACCESO-ARCHIVO.
       RECORD KEY IS CLAVE-REGISTRO.
       ALTERNATE RECORD KEY IS CLAVES-ALTERNATIVAS-REGISTRO.
       WITH DUPLICATES.
       STATUS IS. VARIABLE-ESTADO-ARCHIVO.

      * Sección #3
      DATA DIVISION.

      * Sección #4
      PROCEDURE DIVISION.