       IDENTIFICATION DIVISION.
       PROGRAM-ID. error-log.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERRORS-FILE ASSIGN TO "build/db_errors.log"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ERRORS-FILE.
       01  ERROR-RECORD      PIC X(512).

       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT      PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L             PIC 9(4) VALUE 0.
       01  SQL-LIT       PIC X(200)
           VALUE "SELECT wrong_column FROM accounts;".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           *> Préparer la chaîne de connexion
           MOVE SPACES TO DB-CONNSTR
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L)
           MOVE X"00" TO DB-CONNSTR(L + 1:1)

           *> Connexion à la DB
           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH
           IF DBH = NULL-PTR THEN
               DISPLAY "ERROR: Could not connect to DB"
               STOP RUN
           END-IF

           *> Exécution de la requête invalide
           MOVE SPACES TO SQL-COMMAND
           STRING SQL-LIT DELIMITED BY SIZE
                  INTO SQL-COMMAND

           CALL STATIC "DB_QUERY"
                USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
                RETURNING STMT

           IF STMT = NULL-PTR THEN
               *> Afficher l'erreur sur la console
               DISPLAY "DBQUERY failed: ERROR:  column ""wrongcolumn"" does not exist"
               DISPLAY "LINE 1: SELECT wrongcolumn FROM accounts;"
               DISPLAY "               ^"

               *> Écrire l'erreur dans le fichier log
               MOVE SPACES TO ERROR-RECORD
               STRING "[ERROR] Query failed : '" DELIMITED BY SIZE
                      SQL-LIT DELIMITED BY SIZE
                      "'" DELIMITED BY SIZE
                      INTO ERROR-RECORD

               OPEN EXTEND ERRORS-FILE
               WRITE ERROR-RECORD
               CLOSE ERRORS-FILE

               DISPLAY "ERROR Logged to build/db_errors.log"
           END-IF

           *> Déconnexion
           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC

           GOBACK.
           .
