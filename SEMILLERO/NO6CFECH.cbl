      *----------------------------------------------------------------*
      * OBJETIVO: RUTINA QUE DEVUELVE LA HORA DEL SISTEMA EN VARIOS    *
      * FORMATOS                                                       *
      *----------------------------------------------------------------*
      * FORMATO FECHAS:                                                *
      * 1) DDMMAA                                                      *
      * 2) DDMMAAAA                                                    *
      * 3) DD-MM-AA                                                    *
      * 4) DD-MM-AAAA                                                  *
      * 5) DD/MM/AA                                                    *
      * 6) DD/MM/AAAA                                                  *
      * 7) DD/JUL/AAAA (MES EN LETRAS (3))                             *
      * 8) DD-JUL-AAAA (MES EN LETRAS (3))                             *
      * FORMATO HORA:                                                  *
      * 9) DD-JUL-AAAA (MES EN LETRAS (3))                             *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                       CODIGOS DE RETORNO                       *
      *----------------------------------------------------------------*
      * 00: EXITOSO
      * 01: FORMATO NO VALIDO

      *----------------------------------------------------------------*
      *                           IDENTIFICATION                       *
      *----------------------------------------------------------------*
       ID DIVISION.
       PROGRAM-ID.                   NO6CFECH.
       AUTHOR.                       NOVATEC SOLUTIONS (EDWIN PAEZ).
       INSTALLATION.                 BBVA.
       DATE-WRITTEN.                 18-JUL-22.

      *----------------------------------------------------------------*
      *                           ENVIRONMENT                          *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

      *----------------------------------------------------------------*
      *                           DATA                                 *
      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * OBTENER FECHAS Y HORAS ACTUALES 
       01  WS-FECHA-ACT               PIC 9(06) VALUE ZEROS.    
       01  WS-HORA-ACT                PIC 9(08) VALUE ZEROS.

      * SWITCH PARA VALIDAR SI LAS ENTRADAS SON CORRECTAS
       01  WS-CORRECTO               PIC X VALUE 'N'.
           88 SW-INCORRECTO          VALUE 'N'.
           88 SW-CORRECTO            VALUE 'S'.   

      * FORMATOS:
      * 1) HORA DEL SISTEMA:
        01 WS-HORA-SIS.
           02 WS-HOR-SIS             PIC 9(02) VALUE ZEROS.
           02 FILLER                 PIC X(01) VALUE ':'.
           02 WS-MIN-SIS             PIC 9(02) VALUE ZEROS.
           02 FILLER                 PIC X(01) VALUE ':'.
           02 WS-SEG-SIS             PIC 9(02) VALUE ZEROS.

      * TABLA MESES
       01  WS-M                      PIC 99 VALUE ZEROS.
       01  WS-TABLA-MESES.
           02 WS-LISTA-MESES. 
              05 FILLER              PIC X(03) VALUE 'ENE'.
              05 FILLER              PIC X(03) VALUE 'FEB'.
              05 FILLER              PIC X(03) VALUE 'MAR'.
              05 FILLER              PIC X(03) VALUE 'ABR'.
              05 FILLER              PIC X(03) VALUE 'MAY'.
              05 FILLER              PIC X(03) VALUE 'JUN'.
              05 FILLER              PIC X(03) VALUE 'JUL'.
              05 FILLER              PIC X(03) VALUE 'AGO'.
              05 FILLER              PIC X(03) VALUE 'SEP'.
              05 FILLER              PIC X(03) VALUE 'OCT'.
              05 FILLER              PIC X(03) VALUE 'NOV'.
              05 FILLER              PIC X(03) VALUE 'DIV'.
           02 WS-MESES               REDEFINES WS-LISTA-MESES.
              05 MES                 OCCURS 12 TIMES PIC X(03).

       LINKAGE SECTION.
       COPY './COPYS/VARFECHAS.CPY'.

      *----------------------------------------------------------------*
      *                           PROCEDURE                            *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING WS-FECHAS.
       INICIO.
           PERFORM 01-VALIDA-PARAMETROS-ENTRADA
           IF SW-CORRECTO
               PERFORM 02-HALLAR-FORMATO
           END-IF 
           EXIT PROGRAM.

       01-VALIDA-PARAMETROS-ENTRADA.
           IF WS-FORMATO > 0 AND < 9 THEN
               PERFORM 999-HORAS
               SET SW-CORRECTO TO TRUE
           ELSE
               MOVE '01' TO WS-RETORNO-FECHA
               SET SW-INCORRECTO TO TRUE
           END-IF.

       02-HALLAR-FORMATO.
           EVALUATE WS-FORMATO
             WHEN 1 PERFORM 02-01-FORMATO
             WHEN 2 PERFORM 02-02-FORMATO
             WHEN 3 PERFORM 02-03-FORMATO
             WHEN 4 PERFORM 02-04-FORMATO
             WHEN 5 PERFORM 02-05-FORMATO
             WHEN 6 PERFORM 02-06-FORMATO
             WHEN 7
             WHEN 8 PERFORM 02-07-FORMATO
           END-EVALUATE
           MOVE '00' TO WS-RETORNO-FECHA.

      * 1) DDMMAA:
       02-01-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(5:2)
           MOVE WS-FECHA-ACT(3:2)        TO WS-FORMATO-FECHA(3:2)
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2).

      * DATE: AAMMDD
       02-02-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2)
           MOVE WS-FECHA-ACT(3:2)        TO WS-FORMATO-FECHA(3:2)
           MOVE 20                       TO WS-FORMATO-FECHA(5:2)
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(7:2).

       02-03-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2)
           MOVE '-'                      TO WS-FORMATO-FECHA(3:1)
           MOVE WS-FECHA-ACT(3:2)        TO WS-FORMATO-FECHA(4:2)
           MOVE '-'                      TO WS-FORMATO-FECHA(6:1)
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(7:2).

       02-04-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2)
           MOVE '-'                      TO WS-FORMATO-FECHA(3:1)
           MOVE WS-FECHA-ACT(3:2)        TO WS-FORMATO-FECHA(4:2)
           MOVE '-'                      TO WS-FORMATO-FECHA(6:1)
           MOVE 20                       TO WS-FORMATO-FECHA(7:2)
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(9:2).

       02-05-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2)
           MOVE '/'                      TO WS-FORMATO-FECHA(3:1)
           MOVE WS-FECHA-ACT(3:2)        TO WS-FORMATO-FECHA(4:2)
           MOVE '/'                      TO WS-FORMATO-FECHA(6:1)
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(7:2).

       02-06-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2)
           MOVE '/'                      TO WS-FORMATO-FECHA(3:1)
           MOVE WS-FECHA-ACT(3:2)        TO WS-FORMATO-FECHA(4:2)
           MOVE '/'                      TO WS-FORMATO-FECHA(6:1)
           MOVE 20                       TO WS-FORMATO-FECHA(7:2)
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(9:2).

       02-07-FORMATO.
           ACCEPT WS-FECHA-ACT           FROM DATE
           MOVE WS-FECHA-ACT(3:2)        TO WS-M
           MOVE WS-FECHA-ACT(5:2)        TO WS-FORMATO-FECHA(1:2)
           IF WS-FORMATO = 7 THEN
              MOVE '/'                   TO WS-FORMATO-FECHA(3:1)
              MOVE '/'                   TO WS-FORMATO-FECHA(7:1)
           ELSE 
              IF WS-FORMATO = 8 THEN
                MOVE '-'                 TO WS-FORMATO-FECHA(3:1)
                MOVE '-'                 TO WS-FORMATO-FECHA(7:1)
              END-IF
           END-IF
           MOVE MES(WS-M)                TO WS-FORMATO-FECHA(4:3)       
           MOVE 20                       TO WS-FORMATO-FECHA(8:2)
           MOVE WS-FECHA-ACT(1:2)        TO WS-FORMATO-FECHA(10:2).

       999-HORAS.
           ACCEPT WS-HORA-ACT            FROM TIME
           MOVE WS-HORA-ACT(1:2)         TO WS-HOR-SIS
           MOVE WS-HORA-ACT(3:2)         TO WS-MIN-SIS
           MOVE WS-HORA-ACT(5:2)         TO WS-SEG-SIS
           MOVE WS-HORA-SIS              TO WS-FORMATO-HORA.