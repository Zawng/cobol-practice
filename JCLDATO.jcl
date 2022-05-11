//HERC01DT JOB (COBOL),                                      
//             'COMPILADOR COBOL',                           
//             CLASS=A,NOTIFY=HERC01,                        
//*            TYPRUN=SCAN,                                  
//             MSGCLASS=H,MSGLEVEL=(1,1),                    
//             REGION=8M,TIME=1440                           
//COMPILE  EXEC COBOL,                                       
//         PROG='HELLO',                                     
//         PDSF='HERC01.NOVATEC.SRC',                         
//         PDSL='HERC01.NOVATEC.LOAD'                         
//PASO01   EXEC PGM=HELLO
//STEPLIB  DD DSN=HERC01.NOVATEC.LOAD,DISP=SHR                
//SYSOUT   DD SYSOUT=*                                       
/*   