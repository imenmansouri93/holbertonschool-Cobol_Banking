       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-BALANCES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN
               DISPLAY "Connection failed!"
               STOP RUN
           END-IF.

           MOVE "SELECT account_id, balance, customer_id FROM accounts;" TO SQL-COMMAND.
           MOVE X"00" TO SQL-COMMAND(FUNCTION LENGTH(FUNCTION TRIM(SQL-COMMAND)) + 1:1).

           CALL STATIC "DB_QUERY" USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING STMT.
           IF STMT = NULL-PTR THEN
               DISPLAY "Query failed!"
               STOP RUN
           END-IF.

           PERFORM UNTIL RC NOT = 0
               CALL STATIC "DB_FETCH" USING BY VALUE STMT, BY REFERENCE C1, C2, C3 RETURNING RC
               IF RC = 0 THEN
                   DISPLAY "-> Account " FUNCTION TRIM(C1) ", balance " FUNCTION TRIM(C2)
               END-IF
           END-PERFORM.

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
           DISPLAY "--- End of Task 1 ---".
           GOBACK.
