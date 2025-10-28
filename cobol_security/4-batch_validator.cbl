       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-VALIDATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT TXN-FILE ASSIGN TO "TRANSACTIONS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNTS-FILE.
       01 ACCOUNT-RECORD.
           05 ACCT-ID      PIC X(10).
           05 ACCT-STATUS  PIC X(6).
           05 ACCT-BALANCE PIC 9(7)V99.
       FD TXN-FILE.
       01 TXN-RECORD.
           05 TXN-TYPE     PIC X.
           05 TXN-SRC      PIC X(10).
           05 TXN-DEST     PIC X(10).
           05 TXN-AMOUNT   PIC 9(7)V99.
           05 TXN-DESC     PIC X(30).
           05 TXN-AUTH     PIC X(6).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC XX.
       01 EOF-ACCTS      PIC X VALUE 'N'.
       01 EOF-TXNS       PIC X VALUE 'N'.
       01 TXN-STATUS     PIC X(40).
       01 I              PIC 9(2).
       01 CHAR           PIC X.
       01 IDX-SRC        PIC 9(2) VALUE 0.
       01 IDX-DEST       PIC 9(2) VALUE 0.
       01 TXN-COUNT      PIC 9(3) VALUE 0.
       01 ACCTS-LOADED   PIC 9(3) VALUE 0.

       01 T-ACCT-ID      OCCURS 50 TIMES PIC X(10).
       01 T-ACCT-STATUS  OCCURS 50 TIMES PIC X(6).
       01 T-ACCT-BALANCE OCCURS 50 TIMES PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "SECURE BATCH TRANSACTION VALIDATOR"
           *> Load accounts
           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL EOF-ACCTS = 'Y'
               READ ACCOUNTS-FILE
                   AT END MOVE 'Y' TO EOF-ACCTS
                   NOT AT END
                       ADD 1 TO ACCTS-LOADED
                       MOVE ACCT-ID TO T-ACCT-ID(ACCTS-LOADED)
                       MOVE ACCT-STATUS TO T-ACCT-STATUS(ACCTS-LOADED)
                       MOVE ACCT-BALANCE TO T-ACCT-BALANCE(ACCTS-LOADED)
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE
           DISPLAY ACCTS-LOADED " accounts loaded"

           *> Process transactions
           OPEN INPUT TXN-FILE
           MOVE 0 TO TXN-COUNT
           PERFORM UNTIL EOF-TXNS = 'Y'
               READ TXN-FILE
                   AT END MOVE 'Y' TO EOF-TXNS
                   NOT AT END
                       ADD 1 TO TXN-COUNT
                       PERFORM PROCESS-TXN
           END-PERFORM
           CLOSE TXN-FILE

           STOP RUN.

       PROCESS-TXN.
           MOVE SPACES TO TXN-STATUS
           DISPLAY " "
           DISPLAY "Txn " TXN-COUNT ":"

           *> Validate description
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
               MOVE TXN-DESC(I:1) TO CHAR
               IF CHAR = "'" OR CHAR = ";" OR CHAR = "-"
                   MOVE "REJECTED - Suspicious description"
                        TO TXN-STATUS
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Find source account
           MOVE 0 TO IDX-SRC
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCTS-LOADED
               IF T-ACCT-ID(I) = TXN-SRC
                   MOVE I TO IDX-SRC
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Find destination account
           MOVE 0 TO IDX-DEST
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCTS-LOADED
               IF T-ACCT-ID(I) = TXN-DEST
                   MOVE I TO IDX-DEST
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Validate source account
           IF (TXN-TYPE = "T" OR TXN-TYPE = "W") AND IDX-SRC > 0
               IF T-ACCT-STATUS(IDX-SRC) = "LOCKED"
                   MOVE "REJECTED - Source account locked"
                        TO TXN-STATUS
               ELSE
                   IF TXN-AMOUNT > T-ACCT-BALANCE(IDX-SRC)
                       MOVE "REJECTED - Insufficient funds"
                            TO TXN-STATUS
                   END-IF
               END-IF
           END-IF

           *> Validate destination account
           IF (TXN-TYPE = "T" OR TXN-TYPE = "D") AND IDX-DEST > 0
               IF T-ACCT-STATUS(IDX-DEST) = "LOCKED"
                   MOVE "REJECTED - Destination account locked"
                        TO TXN-STATUS
               END-IF
           END-IF

           *> Approve if still empty
           IF TXN-STATUS = SPACES
               MOVE "STATUS: APPROVED" TO TXN-STATUS
           END-IF

           DISPLAY TXN-STATUS.
