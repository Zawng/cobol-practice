      *-----------------------------------------------------
      *--------- CALCULADORA DE INTERES COMPUESTO ----------
      *-----------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     PRACTICE.
       AUTHOR.                         EDWIN.
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------
      * VARIABLES
      *-----------------------------------------------------
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
      *VARIABLES PARA HALLAR EL INTERÉS COMPUESTO                       
       01  WSV-VARIABLES.
           05 WSV-DEPOSIT                  PIC 9(10) VALUE 1000.       
           05 WSV-CONTRIBUTION             PIC 9(10) VALUE 1000.       
           05 WSV-YEARS                    PIC 9(2) VALUE 3.         
           05 WSV-RATE                     PIC 9(02) VALUE 10.         
           05 WSV-RESULT                   PIC Z(12) VALUE ZEROS.  
      *VARIABLES PARA UTILIDADES     
           05 WSV-TOTAL                    PIC 9(02) VALUE 10.         
           05 WSV-INC                      PIC 9(02) VALUE 0.         
           05 WSV-USER                     PIC X(10) VALUE 'EDWIN PAEZ'.  
       PROCEDURE DIVISION.
      *PROCESO PRINCIPAL 
           ADD 1 TO WSV-YEARS.
           PERFORM Z-FIND-INTERES UNTIL WSV-INC = WSV-YEARS.
       Z-FIND-INTERES.
           PERFORM Z-DISPLAYS.
           ADD 1 TO WSV-INC.
       Z-DISPLAYS.
           DISPLAY 'ITERATION NUMBER ' WSV-INC.
       Z-RETURN.
           STOP RUN.
        