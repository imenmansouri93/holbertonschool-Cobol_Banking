       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATER.
       AUTHOR. HOLBERTON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS ASSIGN TO 'ACCOUNTS.MASTER'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSIN ASSIGN TO 'TRANSACTIONS.VALIDATED'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSOUT ASSIGN TO 'ACCOUNTS.UPDATED'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ACCOUNTS.
       01 ACC-FILE-REC.
           05 ACC-ID       PIC 9(5).
           05 ACC-NAME     PIC X(20).
           05 ACC-TYPE     PIC X(8).
           05 ACC-BAL      PIC 9(8)V99.

       FD TRANSIN.
       01 TRANS-REC.
           05 TXN-ID       PIC X(6).
           05 TXN-TYPE     PIC X(10).
           05 TXN-ACC-ID   PIC 9(5).
           05 TXN-AMT      PIC 9(8)V99.
           05 TXN-DATE     PIC 9(8).

       FD TRANSOUT.
       01 OUT-FILE-REC   PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-END-ACC       PIC X VALUE 'N'.
       01 WS-END-TRANS     PIC X VALUE 'N'.
       01 WS-TOTAL-UPDATES PIC 9(4) VALUE 0.
       01 WS-DEP           PIC 9(4) VALUE 0.
       01 WS-WITH          PIC 9(4) VALUE 0.
       01 WS-TRANSF        PIC 9(4) VALUE 0.
       01 WS-FAILED        PIC 9(4) VALUE 0.
       01 WS-DISPLAY-LINE  PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT ACCOUNTS
                INPUT TRANSIN
                OUTPUT TRANSOUT

           PERFORM READ-ACCOUNTS

           CLOSE ACCOUNTS TRANSIN TRANSOUT

           DISPLAY 'Processing completed. Total updates: ' 
                WS-TOTAL-UPDATES

           DISPLAY 'Deposits: ' WS-DEP 
                ' Withdrawals: ' WS-WITH 
                ' Transfers: ' WS-TRANSF

           DISPLAY 'Failed transactions: ' WS-FAILED

           STOP RUN.

       READ-ACCOUNTS.
           READ ACCOUNTS
               AT END MOVE 'Y' TO WS-END-ACC
           END-READ
           PERFORM UNTIL WS-END-ACC = 'Y'
               PERFORM PROCESS-TRANS
               STRING ACC-ID ',' ACC-NAME ',' ACC-TYPE ',' ACC-BAL
                   DELIMITED BY SIZE
                   INTO OUT-FILE-REC
               END-STRING
               WRITE OUT-FILE-REC
               READ ACCOUNTS
                   AT END MOVE 'Y' TO WS-END-ACC
               END-READ
           END-PERFORM.

       PROCESS-TRANS.
           MOVE 'N' TO WS-END-TRANS
           PERFORM UNTIL WS-END-TRANS = 'Y'
               READ TRANSIN
                   AT END MOVE 'Y' TO WS-END-TRANS
               END-READ
               IF WS-END-TRANS NOT = 'Y'
                   IF TXN-ACC-ID = ACC-ID
                       EVALUATE TXN-TYPE
                           WHEN 'DEPOSIT   '
                               ADD TXN-AMT TO ACC-BAL
                               ADD 1 TO WS-DEP
                               ADD 1 TO WS-TOTAL-UPDATES
                           WHEN 'WITHDRAWAL'
                               SUBTRACT TXN-AMT FROM ACC-BAL
                               ADD 1 TO WS-WITH
                               ADD 1 TO WS-TOTAL-UPDATES
                           WHEN 'TRANSFER  '
                               SUBTRACT TXN-AMT FROM ACC-BAL
                               ADD 1 TO WS-TRANSF
                               ADD 1 TO WS-TOTAL-UPDATES
                           WHEN OTHER
                               ADD 1 TO WS-FAILED
                               DISPLAY 'Failed transaction: ' TXN-ID
                                   ' Type: ' TXN-TYPE
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.
