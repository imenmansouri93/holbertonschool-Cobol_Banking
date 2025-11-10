       IDENTIFICATION DIVISION.
       PROGRAM-ID. process-transactions.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTIONS-FILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "transactions.dat".
       01  TRANSACTION-REC  PIC X(200).

       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".

       01  CONN-LIT  PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L-CONN   PIC 9(4) VALUE 0.
       01  L-SQL    PIC 9(4) VALUE 0.
       01  DUMMY-RC PIC S9(9) COMP-5.
       01  LINE-DATA  PIC X(200) VALUE SPACES.
       01  ACTION     PIC X(10) VALUE SPACES.
       01  CUST-NAME  PIC X(64) VALUE SPACES.
       01  ACCT-ID    PIC 9(6) VALUE 0.
       01  AMOUNT     PIC 9(8)V99 VALUE 0.
       01  FILE-STATUS PIC XX.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *> Connexion
           MOVE SPACES TO DB-CONNSTR
           COMPUTE L-CONN = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))
           MOVE CONN-LIT(1:L-CONN) TO DB-CONNSTR(1:L-CONN)
           MOVE X"00" TO DB-CONNSTR(L-CONN+1:1)
           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH
           IF DBH = NULL-PTR THEN
               DISPLAY "Error: Unable to connect to the database."
               STOP RUN
           END-IF

           *> Ouvrir le fichier
           OPEN INPUT TRANSACTIONS-FILE
           PERFORM UNTIL FILE-STATUS = "10"
               READ TRANSACTIONS-FILE INTO LINE-DATA
                   AT END
                       MOVE "10" TO FILE-STATUS
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE TRANSACTIONS-FILE

           *> Déconnexion
           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING DUMMY-RC
           GOBACK.

       PROCESS-LINE.
           UNSTRING LINE-DATA DELIMITED BY "," 
               INTO ACTION, CUST-NAME, ACCT-ID, AMOUNT

           EVALUATE FUNCTION TRIM(ACTION)
               WHEN "INSERT"
                   *> Insert customer
                   STRING "INSERT INTO customers(name) VALUES('" CUST-NAME "')" 
                          DELIMITED BY SIZE INTO SQL-COMMAND
                   CALL STATIC "DB_EXECUTE" USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING DUMMY-RC

                   *> Insert account
                   STRING "INSERT INTO accounts(customer_id, balance) "
                          "VALUES((SELECT MAX(id) FROM customers), " AMOUNT ")" 
                          DELIMITED BY SIZE INTO SQL-COMMAND
                   CALL STATIC "DB_EXECUTE" USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING DUMMY-RC
                   DISPLAY "Processed INSERT for " CUST-NAME

               WHEN "DEPOSIT"
                   STRING "UPDATE accounts SET balance = balance + " AMOUNT
                          " WHERE account_id = " ACCT-ID
                          DELIMITED BY SIZE INTO SQL-COMMAND
                   CALL STATIC "DB_EXECUTE" USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING DUMMY-RC
                   DISPLAY "Processed DEPOSIT for account " ACCT-ID

               WHEN "WITHDRAW"
                   STRING "UPDATE accounts SET balance = balance - " AMOUNT
                          " WHERE account_id = " ACCT-ID
                          DELIMITED BY SIZE INTO SQL-COMMAND
                   CALL STATIC "DB_EXECUTE" USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING DUMMY-RC
                   DISPLAY "Processed WITHDRAW for account " ACCT-ID
           END-EVALUATE.
