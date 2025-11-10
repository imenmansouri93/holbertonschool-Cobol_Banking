       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-TRANSACTIONS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTIONS-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTIONS-FILE.
       01  TRANSACTION-REC        PIC X(256).

       WORKING-STORAGE SECTION.
       *> Database variables
       01  DB-CONNSTR              PIC X(256).
       01  SQL-COMMAND             PIC X(512).
       01  DBH                     USAGE POINTER.
       01  STMT                    USAGE POINTER.
       01  NULL-PTR                USAGE POINTER.
       01  RC                      PIC S9(9) COMP-5.

       *> Transaction fields
       01  ACTION                  PIC X(10).
       01  NAME                    PIC X(64).
       01  ACCOUNT-ID              PIC 9(6).
       01  AMOUNT                  PIC 9(10)V99.

       01  L-TRAN                  PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *> Connect to database
           MOVE SPACES TO DB-CONNSTR
           STRING "host=127.0.0.1 dbname=schooldb user=postgres password=postgres"
               DELIMITED BY SIZE
               INTO DB-CONNSTR
           END-STRING.

           CALL "DB_CONNECT" USING BY VALUE DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR
               DISPLAY "Error: Unable to connect to database."
               STOP RUN
           END-IF.

           *> Open transactions file
           OPEN INPUT TRANSACTIONS-FILE
           IF RC NOT = 0
               DISPLAY "Error: Cannot open transactions.dat"
               CALL "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC
               STOP RUN
           END-IF.

           PERFORM UNTIL RC NOT = 0
               READ TRANSACTIONS-FILE INTO TRANSACTION-REC
                   AT END MOVE 1 TO RC
               END-READ
               IF RC = 0
                   PERFORM PROCESS-LINE
               END-IF
           END-PERFORM.

           CLOSE TRANSACTIONS-FILE
           CALL "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC
           GOBACK.

       PROCESS-LINE.
           *> Split line into fields ACTION;NAME;ACCOUNT-ID;AMOUNT
           UNSTRING TRANSACTION-REC
               DELIMITED BY ";"
               INTO ACTION NAME ACCOUNT-ID AMOUNT
           END-UNSTRING.

           EVALUATE ACTION
               WHEN "INSERT"
                   *> Insert into customers
                   STRING "INSERT INTO customers (name) VALUES ('"
                          NAME
                          "')" DELIMITED BY SIZE
                          INTO SQL-COMMAND
                   END-STRING.
                   CALL "DB_EXECUTE" USING BY VALUE DBH SQL-COMMAND RETURNING STMT
                   *> Retrieve last inserted customer id
                   CALL "DB_GET_LAST_ID" USING BY VALUE DBH RETURNING ACCOUNT-ID

                   *> Insert into accounts
                   STRING "INSERT INTO accounts (customer_id, balance) VALUES ("
                          ACCOUNT-ID
                          ", "
                          AMOUNT
                          ")" DELIMITED BY SIZE
                          INTO SQL-COMMAND
                   END-STRING.
                   CALL "DB_EXECUTE" USING BY VALUE DBH SQL-COMMAND RETURNING STMT
                   DISPLAY "Processed INSERT for " NAME

               WHEN "UPDATE"
                   EVALUATE TRUE
                       WHEN ACCOUNT-ID > 0 AND AMOUNT > 0
                           *> Determine deposit or withdraw from NAME field (or extra field)
                           *> Here we assume NAME field contains DEPOSIT/WITHDRAW
                           IF NAME = "DEPOSIT"
                               STRING "UPDATE accounts SET balance = balance + "
                                      AMOUNT
                                      " WHERE account_id = "
                                      ACCOUNT-ID
                                      DELIMITED BY SIZE
                                      INTO SQL-COMMAND
                               END-STRING.
                               CALL "DB_EXECUTE" USING BY VALUE DBH SQL-COMMAND RETURNING STMT
                               DISPLAY "Processed DEPOSIT for account " ACCOUNT-ID
                           ELSE
                               STRING "UPDATE accounts SET balance = balance - "
                                      AMOUNT
                                      " WHERE account_id = "
                                      ACCOUNT-ID
                                      DELIMITED BY SIZE
                                      INTO SQL-COMMAND
                               END-STRING.
                               CALL "DB_EXECUTE" USING BY VALUE DBH SQL-COMMAND RETURNING STMT
                               DISPLAY "Processed WITHDRAW for account " ACCOUNT-ID
                           END-IF
                   END-EVALUATE
               WHEN OTHER
                   DISPLAY "Unknown action: " ACTION
           END-EVALUATE.

           MOVE SPACES TO ACTION NAME ACCOUNT-ID AMOUNT
           .
