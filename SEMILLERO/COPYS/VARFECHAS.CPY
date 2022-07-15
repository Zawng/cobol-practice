       01  WS-FECHA-ACT               PIC 9(06) VALUE ZEROS.
       01  WS-HORA-ACT                PIC 9(08) VALUE ZEROS.
       01  WS-FECHA-SIS.
           02 WS-DIA-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE '/'.
           02 WS-MES-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE '/'.
           02 WS-SIG-SIS              PIC 9(02) VALUE 20.
           02 WS-ANO-SIS              PIC 9(02) VALUE ZEROS.

       01  WS-HORA-SIS.
           02 WS-HOR-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE ':'.
           02 WS-MIN-SIS              PIC 9(02) VALUE ZEROS.
           02 FILLER                  PIC X(01) VALUE ':'.
           02 WS-SEG-SIS              PIC 9(02) VALUE ZEROS.
