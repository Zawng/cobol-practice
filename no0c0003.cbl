      * Capture un nombre, que salga linea 2: fecha del sistema 
      * 3: hora del sistema
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                       NO0C0003.
       AUTHOR.                           NOVATEC (EDWIN PAEZ).
       INSTALLATION.                     BBVA.
       DATE-WRITTEN.                     06-06-22.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * VARIABLES DE ENTRADA 
       01  WS-VARIABLES.
           02 WS-NOMBRE                  PIC X(30) VALUE SPACES.
           02 WS-PAUSA                   PIC X     VALUE SPACE.
           02 WS-FECHA                   PIC 9(6)  VALUE ZEROES.
           02 WS-HORA                    PIC 9(8)  VALUE ZEROES.
      
      * VARIABLES DE SALIDA
      * 
           02 WS-FECHA-SAL.
              03 WS-DIA-SAL              PIC 9(2) VALUE ZEROES.  
              03 WS-SEPA-01              PIC X    VALUE '/'.
              03 WS-MES-SAL              PIC 9(2) VALUE ZEROES.  
              03 WS-SEPA-02              PIC X    VALUE '/'.
              03 WS-SIGLO-SAL            PIC 9(2) VALUE 20.  
              03 WS-ANO-SAL              PIC 9(2) VALUE ZEROES.  

           02 WS-HORA-SAL.
              03 WS-HOR-SAL              PIC 9(2) VALUE ZEROES.
              03 WS-SEP-01               PIC X VALUE ':'.
              03 WS-MIN-SAL              PIC 9(2) VALUE ZEROES.
              03 WS-SEP-01               PIC X VALUE ':'.
              03 WS-SEG-SAL              PIC 9(2) VALUE ZEROES.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY ERASE
      * OBTENGO LAS ENTRADAS
      *    FORMATO: 220606     
           ACCEPT WS-FECHA FROM DATE
           MOVE WS-FECHA(5:2) TO WS-DIA-SAL
           MOVE WS-FECHA(3:2) TO WS-MES-SAL
           MOVE WS-FECHA(1:2) TO WS-ANO-SAL

      *    FORMATO: 09012856     
           ACCEPT WS-HORA FROM TIME
           MOVE WS-HORA(1:2) TO WS-HOR-SAL
           MOVE WS-HORA(3:2) TO WS-MIN-SAL
           MOVE WS-HORA(5:2) TO WS-SEG-SAL

      * SOLICITAR INFORMACION DEL USUARIO
           DISPLAY 'SISTEMA DE USUARIOS' LINE 01 POSITION 30
           DISPLAY 'INGRESE SU NOMBRE: ' LINE 02 POSITION 01              
           ACCEPT WS-NOMBRE              LINE 02 POSITION 20
           DISPLAY ERASE
       
      * MOSTRAR LA FECHA Y HORA EN PANTALLA
           DISPLAY 'FECHA DEL SISTEMA: ' LINE 02 POSITION 01
           DISPLAY WS-FECHA-SAL          LINE 02 POSITION 20
           DISPLAY 'HORA DEL SISTEMA: '  LINE 02 POSITION 50
           DISPLAY WS-HORA-SAL           LINE 02 POSITION 69

      * MENSAJE DEL USUARIO
           DISPLAY WS-NOMBRE             LINE 12 POSITION 27
           
      * MENSAJE DE SALIDA
           DISPLAY '<OPRIMA ENTER>'      LINE 24 POSITION 33
           ACCEPT WS-PAUSA               LINE 24 POSITION 48
           STOP RUN.
