      *PRIMERA SECCION COBOL - INFORMACION DEL MISMO
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     CONDITIO.
      *SEGUNDA SECCION COBOL - DATOS DE ENTRADA
       ENVIRONMENT DIVISION.
      *TERCERA SECCION COBOL - VARIABLES
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WSC-CONSTANTES.
           05 WSC-A                     PIC  9 VALUE 7.
           05 WSC-B                     PIC  9 VALUE 9.
      *CUARTA SECCION - PROCESOS
       PROCEDURE DIVISION.
      *CONDICIONALES IF ELSE
           PERFORM 010-IF.
           PERFORM 020-FIN.
       010-IF.
           IF WSC-A < WSC-B
               DISPLAY 'EL VALOR DE: ' WSC-A ' ES MENOR QUE: ' WSC-B
           ELSE
               DISPLAY 'EL VALOR DE: ' WSC-A ' ES MAYOR QUE: ' WSC-B
       020-FIN.
           STOP RUN.
