      ****************************************************************** 
      * CALCULADORA DE INTERES COMPUESTO
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                          PRACTICE.
       AUTHOR.                              EDWIN.
       ENVIRONMENT DIVISION.
      ****************************************************************** 
      * VARIABLE
      ****************************************************************** 
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
      * INTERÉS COMPUESTO                       
       01  WSV-VARIABLES.
           05 WSV-DEPOSIT                  PIC 9(10) VALUE 1000.       
           05 WSV-CONTRIBUTION             PIC 9(10) VALUE 1000.       
           05 WSV-YEARS                    PIC 9(2) VALUE 3.         
           05 WSV-RATE                     PIC 9V9 VALUE 0.1.         
      * UTILIDADES     
           05 WSV-TOTAL                    PIC Z.9(17) VALUE ZEROS.         
           05 WSV-HLP                      PIC ZZZ.9 VALUE ZEROS.         
       PROCEDURE DIVISION.
      ****************************************************************** 
      * PROCESO PRINCIPAL
      ****************************************************************** 
       Z-MAIN.
           PERFORM Z-COMPOUND.
      ****************************************************************** 
      * PROCESO PARA HALLAR EL INTERES COMPUESTO
      ****************************************************************** 
       Z-COMPOUND.
           MOVE WSV-DEPOSIT TO WSV-TOTAL. 
           PERFORM Z-FIND-INTERES WSV-YEARS TIMES
           PERFORM Z-DISPLAYS.
           PERFORM Z-RETURN.
      ****************************************************************** 
      * ITERACIÓN AL PROCESO MATEMATICO
      ****************************************************************** 
       Z-FIND-INTERES.
           PERFORM Z-MATH.
      ****************************************************************** 
      * PROCESO MATEMATICO
      ****************************************************************** 
       Z-MATH.
           COMPUTE WSV-HLP = 1 + WSV-RATE.
           COMPUTE WSV-TOTAL = (WSV-TOTAL + WSV-CONTRIBUTION) * WSV-HLP. 
           DISPLAY 'WSV-HLP: ' WSV-HLP.
           DISPLAY 'WSV-TOTAL: ' WSV-TOTAL.

      ****************************************************************** 
      * IMPRESIONES EN PANTALLA
      ****************************************************************** 
       Z-DISPLAYS.
           DISPLAY 'INTERES COMPUESTO: ' WSV-TOTAL.
      ****************************************************************** 
      * RETORNO
      ****************************************************************** 
       Z-RETURN.
           STOP RUN.