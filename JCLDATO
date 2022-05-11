//* JCL para compilar y ejecutar desde un solo JOB
//HERC01DT JOB (COBOL),                                      
//             'COMPILADOR COBOL',                           
//             CLASS=A,NOTIFY=HERC01,                        
//*            TYPRUN=SCAN,                                  
//             MSGCLASS=H,MSGLEVEL=(1,1),                    
//             REGION=8M,TIME=1440     
//* Compila el archivo                      
//COMPILE  EXEC COBOL,                                       
//         PROG='HELLO',                                     
//         PDSF='HERC01.NOVATEC.SRC',                         
//         PDSL='HERC01.NOVATEC.LOAD'    
//* Ejecuta el archivo                                        
//PASO01   EXEC PGM=HELLO
//STEPLIB  DD DSN=HERC01.NOVATEC.LOAD,DISP=SHR                
//SYSOUT   DD SYSOUT=*                                       
/*   