      *-----------------------------------------------------
      *--------- CALCULADORA DE INTERES COMPUESTO ----------
      *-----------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                          PRACTICE.
       AUTHOR.                              EDWIN.
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
           05 WSV-RATE                     PIC 9V9 VALUE 0.1.         
           05 WSV-RESULT                   PIC Z(12) VALUE ZEROS.  
      *VARIABLES PARA UTILIDADES     
           05 WSV-TOTAL                    PIC 9(02) VALUE ZEROS.         
           05 WSV-INC                      PIC 9(02) VALUE 0.         
           05 WSV-USER                     PIC X(10) VALUE 'EDWIN PAEZ'.  
       PROCEDURE DIVISION.
      *PROCESO PRINCIPAL 
       Z-MAIN.
           MOVE WSV-DEPOSIT TO WSV-TOTAL. 
           PERFORM Z-FIND-INTERES UNTIL WSV-INC = WSV-YEARS.
           PERFORM Z-DISPLAYS.
           PERFORM Z-RETURN.
       Z-FIND-INTERES.
           PERFORM Z-MATH.
           ADD 1 TO WSV-INC.
       Z-MATH.
           COMPUTE WSV-TOTAL = (WSV-TOTAL + WSV-CONTRIBUTION) * (1 + WSV
      -    -RATE).
       Z-DISPLAYS.
           MOVE WSV-TOTAL TO WSV-RESULT.
           DISPLAY 'INTERES COMPUESTO: ' WSV-RESULT.
           DISPLAY 'THIS PROGRAM WAS WRITTEN BY: ' WSV-USER.
       Z-RETURN.
           STOP RUN.