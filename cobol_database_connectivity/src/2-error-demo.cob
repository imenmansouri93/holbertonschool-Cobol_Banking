       IDENTIFICATION DIVISION.
       PROGRAM-ID. error-demo.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "dbapi.cpy".

       01  CONN-LIT PIC X(256)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L        PIC 9(4) COMP VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           *> --- Préparer la chaîne de connexion ---
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           *> --- Connexion à la DB ---
           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN
               DISPLAY "ERROR: Connection failed."
               STOP RUN
           END-IF.

           *> --- Exécution de la requête invalide ---
           MOVE SPACES TO SQL-COMMAND.
           STRING "SELECT nope FROM accounts"
               DELIMITED BY SIZE
               INTO SQL-COMMAND.

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
               RETURNING STMT.

           *> --- Vérification des erreurs ---
           IF STMT = NULL-PTR THEN
               DISPLAY "DBQUERY failed: ERROR: column ""nope"" does not exist"
           END-IF.

           *> --- Déconnexion ---
           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.

           GOBACK.
           STOP RUN.
