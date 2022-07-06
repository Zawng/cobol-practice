       2001-FECHAS.
           ACCEPT WS-FECHA-ACT           FROM DATE 
           MOVE WS-FECHA-ACT(5:2)        TO WS-DIA-SIS
           MOVE WS-FECHA-ACT(3:2)        TO WS-MES-SIS
           MOVE WS-FECHA-ACT(1:2)        TO WS-ANO-SIS
           ACCEPT WS-HORA-ACT            FROM TIME
           MOVE WS-HORA-ACT(1:2)         TO WS-HOR-SIS
           MOVE WS-HORA-ACT(3:2)         TO WS-MIN-SIS
           MOVE WS-HORA-ACT(5:2)         TO WS-SEG-SIS.

       2002-PANTALLA-FECHAS.
           DISPLAY 'FEC SIS: '           LINE 01 POSITION 01
           DISPLAY WS-FECHA-SIS          LINE 01 POSITION 10          
           DISPLAY 'HORA SIS: '          LINE 01 POSITION 62
           DISPLAY WS-HORA-SIS           LINE 01 POSITION 72.         
