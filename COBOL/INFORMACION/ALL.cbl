
      ******************************************************************
      * Author: EDWIN PAEZ
      * Purpose: NOTES
      ******************************************************************

      ******************************************************************   
      *                           RECORDAR                             *  
      * CONSTANTES FIGURATIVAS: ZERO, ZEROES, ZEROS, SPACE, SPACES, ALL*
      * LOW-VALUES, HIGH-VALUES, QUOTTE, QUOTTES.                      *
      * PALABRAS-DE-USUARIO: 30 DIG, GUIÓN RODEADO, NO CARACTERES.     *
      * PALABRAS RESERVADAS: ACCEPT, DISPLAY, MOVE, ADD ETC.           *
      ******************************************************************   

      ******************************************************************   
      *                            ÁREAS                               *
      * 1 - 6: ENUMERACIÓN AUTOMÁTICA DE LAS LÍNEAS                    *
      * 7: COMENTARIOS Y CONTINUACIONES DE LITERALES                   *
      * 73 - 80: NO SE EJECUTAN.                                       *
      * A: COLUMNAS DE LA 8 - 11.                                      *
      * B: COLUMNAS DE LA 12 - 72.                                     *
      ******************************************************************   
      *----------------------------------------------------------------*
      * CONTIENE INFORMACIÓN DEL PROGAMA, NOMBRE, AUTOR, FECHA DE ESCRI*
      * TURA, FECHA DE COMPILACIÓN Y DESCRIPCIÓN DEL PROGRAMA.         *
      *----------------------------------------------------------------*
       ID DIVISION.                                                     * Obligatorio 
       PROGRAM-ID.                BASE.                                 * Obligatorio - Nombre del progama
       AUTHOR.                    EDWIN-PAEZ.                           * Opcional    - Nombre del autor
       INSTALLATION.              NOVATEC.                              * Opcional    - Donde está alojado el progarma
       DATE-WRITTEN.              16-05-22.                             * Opcional    - Fecha de escritura del programa
       DATE-COMPILED.                                                   * Opcional    - Fecha de compilación del programa
       REMARKS.                   BASE DE PROYECTO COBOL.               * Opcional    - Igual a SECURITY

      *----------------------------------------------------------------*
      * CONTIENE TODOS LOS FICHEROS DE ENTRADA Y SALIDA QUE SERÁN      *
      * USADOS DENTRO DE UN PROGRAMA.                                  * 
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.                                            * Obligatorio
       CONFIGURATION SECTION.                                           * Opcional    - Configuración de entorno
       SOURCE-COMPUTER.           NOVATEC-IBM.                          * Opcional    - Donde se escribió el programa
       OBJECT-COMPUTER.           NOVATEC-IBM.                          * Opcional    - Donde se ejecutará el programa
       SPECIAL-NAMES.                                                   * Opcional    - Cambiar constantes del programa
       DECIMAL-POINT IS COMMA.                                          * Opcional    - Cambiar el punto virtual
       CURRENCY SIGN IS $.                                              * Opcional    - Cambiar el simbolo monetario
       
       INPUT-OUTPUT SECTION.                                            * Opcional    - Definición de archivos
       FILE-CONTROL.                                                    * Opcional    - Sirve para especificar nombres de archivos etc
       SELECT [OPTIONAL] NOMBRE-ARCHIVO                                 * Opcional    - Nombre lógico
       ASSIGN TO TIPO-DE-DISPOSITIVO                                    * Opcional    - [RANDOM, DISC, INPUT, INPUT-OUTPUT, CASSETE, MAGNETIC-TAPE] tipo de dispositivo.
       ORGANIZATION IS TIPO-DE-ORGANIZACION                             * Opcional    - [SEQUENTIAL, RELATIVE, INDEXED] por defecto, secuencial, indica la organización de los registros.
       ACCESS MODE IS MODO-ACCESO-ARCHIVO                               * Opcional    - [SEQUENTIAL, DYNAMIC, RANDOM] modo de acceso al fichero
       RECORD KEY IS CLAVE-REGISTRO                                     * Opcional    - [Indexados] nombre de la clave de tipo X
       ALTERNATE RECORD KEY IS CLAVES-ALTERNATIVAS-REGISTRO             * Opcional    - [Indexados] identifican uan o más claves foráneas
       WITH DUPLICATES                                                  * Opcional    - Existen llaves duplicadas
       STATUS IS VARIABLE-ESTADO-ARCHIVO.                               * Opcional    - [XX] estado del archivo

      *----------------------------------------------------------------*
      * FICHEROS, VARIABLES DEL PROGRAMA, PARAMETROS, COMUNICACION     *
      *----------------------------------------------------------------*
       DATA DIVISION.                                                   * Obligatorio
       FILE SECTION.                                                    * Opcional    - Variables que componen los registros de todos los archivos - INPUT-OUTPUT
       FD NOMBRE-ARCHIVO                                                * Opcional    - Nombre del archivo lógico
       BLOCK CONTAINS [N] RECORDS                                       * Opcional    - Bloques de disco a grabar
       RECORD CONTAINS [N] CHARACTERS                                   * Opcional    - Número totalde caracteres por registro, puede ser fija o variable
       LABEL RECORD [N]                                                 * Opcional    - [STANDAR, OMITTED] tipo de comprobación 
       DATA RECORD NOMBRE-REGISTRO                                      * Opcional    - Descripción del registro, variables, estructura.

       WORKING-STORAGE SECTION.                                         * Opcional Declararación de variables que no tienen nada que ver con archivos
      * Recordar que los nombres de las variables no deben pasar los 
      * 30 caracteres de longitud
      * X => Alfanumérico    - 256 digitos máximo
      * A => Alfabético      - 256 digitos máximo
      * 9 => Números         - 18  digitos máximo
      * V => Punto virtual
      * S => Signo
      * P => Decimal asumido
      * 1 - 77 niveles, variables & constantes 
      * 2 - 49... 88 subniveles, 88 nombres de condición

       77  SW-SWITCHES.                  PIC X(03) VALUE '   '.          * Opcional   - Booleanos
           88 SW-SI.                               VALUE 'FIN'.         
       01  WS-VARIABLES.                                                * Opcional    - Variables
           02 WS-NAME                  PIC X(05) VALUE 'EDWIN'.         
       01  CON-CONSTANTES.                                              * Opcional    - Constantes
           02 CON-TITLE.                                                
               03 FILLER               PIC X(33) VALUE ALL '*'.         * Opcional    - Generar un espacio en memoria sin nombre ocupando constantes figurativas
               03 CON-NAME             PIC X(13) VALUE ' CALCULADORA '.
               03 FILLER               PIC X(34) VALUE ALL '*'.        
      * ACCEPT DE EDAD, ASIGNAR UN VALOR SI ESTÁ EN EL RANGO
       01 EDAD PIC 999.
           88 JOVEN   VALUE 1 THRU 40.
           88 MADURO  VALUE 41 THRU 65.
           88 ANCIANO VALUE 66 THRU 100.
 
       LINKAGE SECTION.                                                 * Opcional    - Manipulación de parámetros

       COMMUNICATION SECTION.                                           * Opcional    - Se usa para la comunicación entre dos programas que se ejecutan simultáneamente

       SCREEN SECTION.                                                  * Opcional    - Atributos a usar en pantallas

      *----------------------------------------------------------------*
      * SE ESCRIBEN TODAS LAS SENTENCIAS COBOL, PÁRRAFOS, RESERVADAS   *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION. USING VARIABLES                              * Obligatorio - El USING es usado en rutinas
       1000-PRINCIPAL.                                                  * Creación de un párrafo
       PERFORM 2001-VERBOS                                              * Ejecución de un párrafo
       PERFORM 3000-FINAL.                                              

       2000-PROCESOS.                                                   
       2001-VERBOS.                                                       
           DISPLAY "HOLA MUNDO"                                         Sentencias
           GO TO 020-HEADER                                             Va al parrafo y no vuelve a su posicion base
           MOVE 10 TO WS-NUM1 WS-NUM2 WS-NUM3 WS-NUM4 WS-NUM5 WS-NUM6.  Mueve un valor a diferentes variables.
           ACCEPT WSV-NUM-1                                             Obtener un valor de una fuente, consola, cobol


      ****************************************************************** 
      *                             CICLOS                             *
      ****************************************************************** 
       2002-CICLOS.
           PERFORM ITERATOR UNTIL WS-NUMBER = 0                         Itera hasta que una variable sea igual que
           PERFORM ITERATOR VARYING WS-NUMBER FROM 10 BY -1 UNTIL       Itera definiendo un inicio y un final, modificando su flujo
           WS-NUMBER < 0
           PERFORM ITERATOR THRU 2001-VERBOS                            Ejecuta dos parrafos
           PERFORM ITERATOR 3 TIMES.                                    Ejecuta 3 veces el parrafo

      ****************************************************************** 
      *                             CONDICIONALES                      *
      ****************************************************************** 
       2003-CONDICIONALES.
           EVALUATE WSV-OPCION                                          Funciona como un switch en otros lenguajes  
               WHEN 0
                   DISPLAY 'APLICACION TERMINADA'
               WHEN 1
                   PERFORM 070-INGRESAR
               WHEN OTHER
                   DISPLAY "OPCION NO ENCONTRADA, FINALIZANDO."
                   STOP RUN
           END-EVALUATE.

           IF LEE-TODO = "1" THEN                                       Condicionales
               DISPLAY "No se encontraron registos en el archivo"
           ELSE
               PERFORM 2003-MUESTRA-CAMPOS UNTIL LEE-TODO = "1"
               PERFORM 2002-CERRAR-ARCHIVO.
           
      ****************************************************************** 
      *                             ARITMETICA                         *
      ****************************************************************** 
       2004-ARITMETICA.
           ADD WSV-NUM-1 TO WSV-NUM-2 GIVING WSV-TOTAL                  Suma
           SUBTRACT WSV-NUM-1 FROM WSV-NUM-2 GIVING WSV-TOTAL           Resta
           MULTIPLY WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL             Multiplicación
           DIVIDE WSV-NUM-1 BY WSV-NUM-2 GIVING WSV-TOTAL               División
           COMPUTE WSV-NUM1 = 1 * 2 * 3 + 4                             Computada

       3000-FINAL.                                                      
           STOP RUN.                                                    * Detener el programa
           END PROGRAM BASE.                                            * Cerrar el programa
       
       4000-RUTINAS.                                                    * Maneras de detener rutinas
           GO BACK
           RETURN
           EXIT PROGRAM

