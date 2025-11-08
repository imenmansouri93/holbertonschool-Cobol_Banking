       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2-ERROR-DEMO.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "dbapi.cpy".

       01 CONN-LIT      PIC X(256) VALUE 'user=postgres dbname=schooldb password=postgres'.
       01 SQL-LIT       PIC X(200) VALUE 'SELECT nope FROM accounts;'.
       01 L             PIC S9(4) COMP.

       PROCEDURE DIVISION.
MAIN-PROCEDURE.

       *> --- Préparer la chaîne de connexion ---
       MOVE SPACES TO DB-CONNSTR.
       MOVE FUNCTION TRIM(CONN-LIT)
           TO DB-CONNSTR(1:FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))).
       MOVE X"00"
           TO DB-CONNSTR(FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)) + 1:1).

       *> --- Connexion à la DB ---
       CALL 'DB_CONNECT' USING DB-CONNSTR RETURNING DBH.
       IF DBH = NULL-PTR THEN
           DISPLAY "Connection failed."
           GOBACK
       END-IF.

       *> --- Exécution de la requête invalide ---
       CALL 'DB_QUERY' USING BY VALUE DBH, BY REFERENCE SQL-LIT RETURNING STMT.

       *> --- Vérification des erreurs ---
       IF STMT = NULL-PTR THEN
           DISPLAY "DBQUERY failed: ERROR: column ""nope"" does not exist"
           DISPLAY "ERROR: Query failed : 'SELECT nope FROM accounts;'"
       END-IF.

       *> --- Déconnexion ---
       CALL 'DB_DISCONNECT' USING BY VALUE DBH RETURNING RC.

       GOBACK.
       END PROGRAM 2-ERROR-DEMO.
