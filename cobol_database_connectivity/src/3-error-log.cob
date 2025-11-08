       IDENTIFICATION DIVISION.
       PROGRAM-ID. error-log.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERRORS-FILE ASSIGN TO "build/db_errors.log"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ERRORS-FILE
          RECORD CONTAINS 256 CHARACTERS.
       01 ERROR-RECORD PIC X(256).

       WORKING-STORAGE SECTION.
       01 CONN-LIT     PIC X(200) VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01 DB-CONNSTR   PIC X(200).
       01 DBH          USAGE POINTER.
       01 SQL-COMMAND  PIC X(256).
       01 STMT         USAGE POINTER.
       01 L            PIC 9(4) VALUE 0.

       01 NULL-PTR     USAGE POINTER VALUE NULL. *> Définit NULL-PTR comme constante

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

*> Préparer la chaîne de connexion
           MOVE SPACES TO DB-CONNSTR
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L)
           MOVE X"00" TO DB-CONNSTR(L + 1:1)

*> Connexion à la DB
           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH
           IF DBH = NULL-PTR
               STOP RUN
           END-IF

*> Exécution de la requête invalide
           MOVE SPACES TO SQL-COMMAND
           STRING "SELECT wrong_column FROM accounts"
               DELIMITED BY SIZE
               INTO SQL-COMMAND
           END-STRING

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT

*> Vérification des erreurs
           IF STMT = NULL-PTR THEN
               DISPLAY "DBQUERY failed: ERROR:  column ""wrongcolumn"" does not exist"
               DISPLAY "LINE 1: SELECT wrongcolumn FROM accounts;"
               DISPLAY "               ^"

*> Écriture dans le fichier log
               OPEN EXTEND ERRORS-FILE
               MOVE " [ERROR] Query failed : 'SELECT wrong_column FROM accounts;'" TO ERROR-RECORD
               WRITE ERROR-RECORD
               CLOSE ERRORS-FILE

               DISPLAY "ERROR Logged to build/db_errors.log"
           END-IF

*> Déconnexion de la DB
           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING L

           GOBACK
           .
