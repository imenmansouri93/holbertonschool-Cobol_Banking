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
       01  ERROR-RECORD    PIC X(512).

       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  SQL-LIT  PIC X(200)
           VALUE "SELECT wrong_column FROM accounts;".
       01  L        PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L)
           MOVE X"00" TO DB-CONNSTR(L + 1:1)

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH
           IF DBH = NULL-PTR THEN
               DISPLAY "Connection failed."
               STOP RUN
           END-IF

           MOVE SPACES TO SQL-COMMAND
           STRING SQL-LIT DELIMITED BY SIZE INTO SQL-COMMAND

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
               RETURNING STMT

           IF STMT = NULL-PTR THEN
               DISPLAY "DBQUERY failed: ERROR:  column ""wrongcolumn"" does not exist"
               DISPLAY "LINE 1: SELECT wrongcolumn FROM accounts;"
               DISPLAY "               ^"

               OPEN EXTEND ERRORS-FILE
               MOVE " [ERROR] Query failed : '" & FUNCTION TRIM(SQL-COMMAND) & "'" TO ERROR-RECORD
               WRITE ERROR-RECORD
               CLOSE ERRORS-FILE

               DISPLAY "ERROR Logged to build/db_errors.log"
           END-IF

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC
           GOBACK.
