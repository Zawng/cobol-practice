# Entorno con Hercules & TK4

**Terminología:**

**Hercules**: emulador del hardware de un mainframe.

**TK4**: emulador del sistema MVS de un mainframe.

**3270**: emulador de terminal IBM.

**Instalaciones**

[Windows](Entorno%20con%20Hercules%20&%20TK4%20fb5e9db0c5934b0182de09224245c305/Windows%20e051d13430d149639ebdf16255f216af.md)

[Ubuntu](Entorno%20con%20Hercules%20&%20TK4%20fb5e9db0c5934b0182de09224245c305/Ubuntu%20038b959da7274c8da0bae9e7012197dd.md)

[Navegación TSO](Entorno%20con%20Hercules%20&%20TK4%20fb5e9db0c5934b0182de09224245c305/Navegacio%CC%81n%20TSO%202dd91b7c77184be49d9f6e5268ed990d.md)

**Iniciar sesión en el 3270**

```bash
# Usuarios por defecto del 3270
HERC01 contraseña CUL8TR = accedo a todo
HERC02 contraseña CUL8TR = casi acceso a todo sin acceso al RAKF
HERC03 contraseña PASS4U = usuario regular
HERC04 contraseña PASS4U = usuario regular
IBMUSER contraseña IBMPASS = propósitos de recuperación
```

**Crear un proyecto de COBOL**

Estructura deseada

Carpetas/PDS

```c
project
|───src // Contiene el código fuente COBOL
|───load  // Contiene los archivos de carga una vez sea compilado
│───jcl // Programas
```

**Creaciones de los DATA SETS**

1. Dentro de MVS - TSO vamos a **RFE**
    1. Opción 1 - RFE
    2. Opción 3 - UTILITIES
    3. Opción 2 - DATASET
    4. Opción A - ALLOCATE NEW DATA SET
    
    Añadir la información del DATA SET - se hace 3 veces, 1 por carpeta
    
    ```bash
    TSO LIBRARY:
    	PROJECT ==> HERC01 # Usuario
    	LIBRARY ==> PLATZI # Nombre de la carpeta root
       	 TYPE ==> SRC # Aquí van las carpetas, SRC, LOAD, JCL
    ```
    
    ```bash
          NAME OF NEW DATA SET ==> 'HERC01.PLATZI.SRC'        
                 RECORD FORMAT ==> FB # Fijo bloqueado
         LOGICAL RECORD LENGTH ==> 80 # Longitud del registro
           PHYSICAL BLOCK SIZE ==> 19040 # Longitud del bloque físico máximo que ocupará en disco nuestra información
                        VOLUME ==> PUB010 # Partición virtual en nuestro disco duro que emula el disco físico que se una en un mainframe.
                          UNIT ==> 
         ALLOCATION SPACE UNIT ==> T
        PRIMARY SPACE QUANTITY ==> 70 # Cantidad de espacio primario en tracks que será ocupado en nuestro disco
      SECONDARY SPACE QUANTITY ==> 5 # Cantidad de espacio secundario en tracks, esto para cuando la cantidad del espacio primario se haya llenado.
    NUMBER OF DIRECTORY BLOCKS ==> 20 # Número de bloques de directorio donde se va a realizar la búsqueda de los componentes con los que estaremos trabajando para la compilación, linkedición y ejecución de nuestros programas.
    ```
    
    Después de crear el primer data set nos paramos en la línea de TSO LIBRARY y presionamos enter dos veces, esto nos copiará la configuración del primer DATA SET y solo debemos cambiar el TYPE por LOAD y JCL respectivamente.
    
    **Editar archivos de los DATS SETS** - guardado automático
    
    1. Dentro de MVS - TSO vamos a **RFE**
        1. Opción 1 - RF3
        2. Opción 2 - EDIT
        3. Opción 3
        
        ```bash
        TSO LIBRARY:
        	PROJECT ===> HERC01
        	LIBRARY ===> PLATZI
        	TYPE    ===> SRC
        	MEMBER  ===> PRUEBA
        
        # Presionamos enter y ya estamos en el modo de edición, aquí es donde aplica la tabla de los atajos
        ```
        
    
    **Implementar el COMPILADOR**
    
    En este caso haremos carga de este archivo llamado PROC_COMPI.txt
    
    ```
    //COBOL PROC SOUT='*'                                                   00000109
    //* ************************************************************        00000209
    //*                                                                     00000309
    //* OBJET. DE PROCEDURE: COMPILAR EL PROGRAMA COBOL, DESPUES HACER      00000409
    //*                      LINKEDIT DEL MODULO DE CARGA.                  00000509
    //*                                                                     00000609
    //*          PARAMETROS: PROG=NOMBRE DE PROGRAMA A COMPILAR             00000709
    //*                      PDSF=LIBRERIA DONDE RESIDE EL CODIGO FUENTE    00000810
    //*                      PDSL=LIBRERIA DONDE RESIDE EL MODULO DE        00000910
    //*                           CARGA                                     00001010
    //* ************************************************************        00001109
    //*                                                                     00001209
    //* PASO IKFCBL01      : COMPILA EL PROGRAMA COBOL                      00001310
    //*                                                                     00001409
    //* ************************************************************        00001509
    //IKFCBL01 EXEC  PGM=IKFCBL00,                                          00001609
    //           PARM='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'              00001709
    //SYSPRINT DD SYSOUT=&SOUT                                              00001809
    //SYSPUNCH DD SYSOUT=*                                                  00001909
    //SYSUT1   DD UNIT=SYSDA,SPACE=(460,(700,100))                          00002009
    //SYSUT2   DD UNIT=SYSDA,SPACE=(460,(700,100))                          00002109
    //SYSUT3   DD UNIT=SYSDA,SPACE=(460,(700,100))                          00002209
    //SYSUT4   DD UNIT=SYSDA,SPACE=(460,(700,100))                          00002309
    //SYSIN    DD DSNAME=&PDSF(&PROG),DISP=SHR                              00002409
    //SYSLIN   DD DSNAME=&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,               00002509
    //            SPACE=(80,(500,100))                                      00002609
    //SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR                               00002709
    //*                                                                     00002809
    //* ************************************************************        00002909
    //*                                                                     00003009
    //* PASO LKED          : SI LA COMPILACION FUE EXITOSA, ESTE PASO       00003110
    //*                      CREA EL MODULO DE CARGA                        00003210
    //*                                                                     00003309
    //* ************************************************************        00003409
    //LKED     EXEC PGM=IEWL,PARM='LIST,XREF,LET',COND=(4,LT,IKFCBL01)      00003510
    //SYSLIN   DD DSNAME=&LOADSET,DISP=(OLD,DELETE)                         00003609
    //         DD DDNAME=SYSIN                                              00003709
    //SYSPUNCH DD SYSOUT=*                                                  00003809
    //SYSLMOD  DD DSNAME=&PDSL(&PROG),DISP=SHR                              00003909
    //SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR                               00004009
    //SYSUT1   DD UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(50,20))    00004109
    //SYSPRINT DD SYSOUT=&SOUT                                              00004209
    //*                                                                     00004309
    //* ************************************************************        00004409
    //* FIN DE PROCEDIMIENTO                                                00004510
    //* ************************************************************        00004609
    ```
    
    **PD**: Las rutas deben ser rutas absolutas, /home/YOURUSERNAME/Downloads/PROC_COMPI.txt
    
    **RECORDAR QUE SOLO PERMITE 8 LETRAS**
    
    1. Dentro de MVS - TSO vamos a RFE
        1. Opción 1 - RFE
        2. Opción 6 - COMMAND
        3. File - File Transfer
        4. Subir el archivo
        
        ```bash
        Enter # 1. 
        send # 2. 
        /home/user/Downloads/PROC_COMPI.txt # ruta debe ser absoluta "archivo a subir"
        'SYS2.PROCLIB(COBOL)' # nombre de archivo al destino
        ... #Desde aquí, enter a todo
        ```
        
    
    Validar si el proceso de la transferencia se culminó satisfactoriamente
    
    1. ahora presionamos F3 hasta estar en el apartado de MVS - TSO
    
    2. Opción 1
    
    3. Opción 3
    
    4. Opción 4
    
    5. Buscamos el proceso
    
    ```bash
    Data set name prefix ==> SYS2.PROCLIB # Buscamos el archivo que acabamos de subir
    Marcar el proceso con la V y enter
    Marcar la fila de COBOL con la V y enter
    ```
    

**Proceso de compilación de programa fuente COBOL**

```bash
1. Programa COBOL
2. Compilador
3. Código objeto
4. Linkedit
5. Módulo de carga

# Proceso de funcionamiento
El código COBOL se pasa al compilador por medio de un JCL # JCL hace referencia a un subprograma PROCEDURE
Si la compilación sale bien se genera un módulo objeto que será tomado por el Linkedit
El linkedit toma el módulo objeto y este genera un programa ejecutable "módulo de carga" # Estará en la carpeta LOAD
```

**Visualización de JOBS**

1. Dentro de MVS - TSO vamos a **RFE**
    1. Opción 1 - RF3
    2. Opción 3 - UTILITIES
    3. Opción 8 - OUTLIST

Si queremos ver todo el contenido de los JCL podemos usar

```bash
COMMAND ===> ST *
```

**Estructura de un JCL - Compilar**

```bash
//HERC01JN JOB (123), # Nombre lógico para detectar el JOB, Parámetro posicional (123)
//             'COMPILAR COBOL', # parámetro de identificación o llave de JCL máximo 20 caracteres
//             CLASS=A, Tomado por el JES y dice el tiempo que tomará
//*            TYPRUN=SCAN, # Evalúa si hay errores de sintaxis y no se ejecuta
//             NOTIFY=HERC01, # Notificará al usuario si salió bien o hay error
//             MSGLEVEL=(1,1), # Hace que se muestren las lineas del JCL, JES y PROC
//             MSGCLASS=H, # La salida en el spool será visualizada para verificar su ejecución,
//             TIME=1440, # 24 horas, tiempo máximo que puede estar en el JES.
//             REGION=8M # Máximo memoria virtual del JES para procesar el JOB
//* ESTA ES LA ESTRUCTURA DE NUESTRO PRIMER JCL # Comentario
//COMPILE EXEC COBOL, 
//             PROG='HOLA', # Nombre del programa COBOL dentro de la carpeta SRC
//             PDSF='HERC01.PLATZI.SRC', # Data set donde está el archivo a ejecutar
//             PDSL='HERC01.PLATZI.LOAD' # Módulo de carga, archivo compilado
/* # Esto termina el proceso del JCL
```

**Estructura de un JCL - Ejecutar**

```bash
//HERC01HJ JOB (COMP), # Nombre lógico para detectar el JOB, Parámetro posicional (123)
//             'EJECUTAR COBOL', # parámetro de identificación o llave de JCL máximo 20 caracteres
//             CLASS=A, # Tomado por el JES y dice el tiempo que tomará
//             NOTIFY=HERC01, # Notificará al usuario si salió bien o hay error
//             MSGLEVEL=(1,1), # Hace que se muestren las lineas del JCL, JES y PROC
//             MSGCLASS=H, # La salida en el spool será visualizada para verificar su ejecución,
//             TIME=1440, # 24 horas, tiempo máximo que puede estar en el JES.
//             REGION=8M # Máximo memoria virtual del JES para procesar el JOB
//* ESTA ES LA ESTRUCTURA DE NUESTRO SEGUNDO JCL # Comentario
//PASO01 EXEC PGM=HOLA # Nombre del programa
//STEPLIB     DD DSN=HERC01.PLATZI.LOAD,DISP=SHR # Módulo de carga, archivo compilado
//SYSOUT     DD SYSOUT=*
/* # Esto termina el proceso del JCL
```

**JCL - Compilar y Ejecutar**

```bash
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
```

**Utilerías - hacen parte de los JCL**

```bash
IEFBR14 # Sirve para eliminar un archivo 
//*-- 
//* DECLARACIÓN DE IEFBR14
//*--
//PASO01 EXEC PGM=IEFBR14 # Ejecutar el programa IEFBR14
//VTAMOBJ DD DSN='HERC01.TEST.ARC' # Crear un archivo, si existe, lo borra y lo crea
//SPACE(TRK, (0)), # Tipo de alojamiento y tamaño del archivo
//UNIT=3570, # Espacio de memoria en disco donde se almacena un archivo
//DISP=(MOD,DELETE), # (0, MOD, DELETE), 0 o null indica archivo nuevo, MOD para que el JES cree el archivo, DELETE para que lo elimine si hay error)
/*
```

```bash
IEBGENER # Respalda archivos de entrada ya existentes en un nuevo archivo - archivos de respaldo
//*-- 
//* DECLARACIÓN DE IEBGENER
//*--
//PASO02 EXEC PGM=IEBGENER,COND=(0,LT) # Llamar a la utilería, evaluando si todo salió bien
//SYSUT1 DD DSN=HERCO1.*,DISP=SHR # Parámetros, tienen DD, en este, DSN data set name, archivo a respaldar 
//SYSUT2 DD DISP=(,MOD,DELETE), # Estado del archivo, si el estado está bien, respalda el archivo, si no, lo elimina
//          DSN=HERC01.*, # Referenciamos el archivo
//          DCB=(*.SYSUT1), # Data control block, hacemos que tome los mismos datos del archivo a respaldar
//          SPACE=(TRK,(200)), # Tamaño del archivo en el disco
//          VOL=SER=PUB010, # Unidad
//          UNIT=3350 # Espacio donde vive la unidad, en el parámetro de VOL
//SYSPRINT DD SYSOUT=0 # DD = data definition, hacia donde se van las lineas de ejecución
//SYSIN DD DUMMY # Datos que va a recibir si el archivo a respaldar necesita nueva información, DUMMY solo toma la info sin hacer cambios
/*
```