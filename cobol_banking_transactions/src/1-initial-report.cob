       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITIAL-REPORT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".

       01 CONN-LIT    PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01 L           PIC 9(4) VALUE 0.
       01 SQL-LEN     PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.

*> Step 1: Prepare connection string
           MOVE SPACES TO DB-CONNSTR
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L)
           MOVE X"00" TO DB-CONNSTR(L + 1:1)

*> Step 2: Connect to DB
           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH
           IF DBH = NULL-PTR THEN
               DISPLAY "Connection failed! Stopping program."
               STOP RUN
           END-IF.

*> Step 3: Prepare SQL query
           MOVE SPACES TO SQL-COMMAND
           MOVE "SELECT c.name, a.balance FROM customers c JOIN accounts a ON c.id = a.customer_id ORDER BY c.id;" 
               TO SQL-COMMAND
           COMPUTE SQL-LEN = FUNCTION LENGTH(FUNCTION TRIM(SQL-COMMAND))
           MOVE X"00" TO SQL-COMMAND(SQL-LEN + 1:1)

*> Step 4: Execute query
           CALL STATIC "DB_QUERY" USING BY VALUE DBH BY REFERENCE SQL-COMMAND RETURNING STMT
           IF STMT = NULL-PTR THEN
               DISPLAY "Query failed! Stopping program."
               CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC
               STOP RUN
           END-IF.

           DISPLAY "--- INITIAL BALANCE REPORT ---"

*> Step 5: Fetch rows
           PERFORM UNTIL RC NOT = 0
               CALL STATIC "DB_FETCH" USING BY VALUE STMT BY REFERENCE C1 BY REFERENCE C2 BY REFERENCE C3 RETURNING RC
               IF RC = 0 THEN
                   DISPLAY "Customer: " FUNCTION TRIM(C1) ", Balance: " FUNCTION TRIM(C2)
               END-IF
           END-PERFORM.

*> Step 6: Disconnect
           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC
           DISPLAY "--- End of Task 1 ---"
           STOP RUN.
