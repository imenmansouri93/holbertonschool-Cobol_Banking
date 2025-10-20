       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE-CUSTOMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUST-ID        PIC 9(5).
           05 CUST-FNAME     PIC X(10).
           05 CUST-LNAME     PIC X(10).
           05 CUST-BALANCE   PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS     PIC XX.
       01 WS-ACCOUNT-ID      PIC X(5).
       01 WS-FIRST-NAME      PIC X(10).
       01 WS-LAST-NAME       PIC X(10).
       01 WS-BALANCE-STR     PIC X(10).
       01 WS-BALANCE         PIC 9(5)V99.
       01 WS-VALID           PIC X VALUE 'N'.
       01 WS-ALPHA-COUNT     PIC 9(2).
       01 WS-LETTERS         PIC X(52) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN I-O CUSTOMER-FILE

           MOVE 'N' TO WS-VALID
           PERFORM UNTIL WS-VALID = 'Y'
               DISPLAY "Enter Account Number (5 digits): "
               ACCEPT WS-ACCOUNT-ID
               IF WS-ACCOUNT-ID = SPACES
                   DISPLAY "  >> Invalid account number."
               ELSE
                   IF FUNCTION LENGTH(WS-ACCOUNT-ID) NOT = 5
                       DISPLAY "  >> Invalid account number."
                   ELSE
                       IF FUNCTION NUMVAL(WS-ACCOUNT-ID) <= 0
                           DISPLAY "  >> Invalid account number."
                       ELSE
                           MOVE WS-ACCOUNT-ID TO CUST-ID
                           READ CUSTOMER-FILE
                               INVALID KEY
                                   MOVE 'Y' TO WS-VALID
                               NOT INVALID KEY
                                   DISPLAY "Account " WS-ACCOUNT-ID
                                   DISPLAY " already exists."
                                   MOVE 'N' TO WS-VALID
                           END-READ
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           MOVE 'N' TO WS-VALID
           PERFORM UNTIL WS-VALID = 'Y'
               DISPLAY "Enter First Name (1-10 letters): "
               ACCEPT WS-FIRST-NAME
               IF WS-FIRST-NAME = SPACES
                   DISPLAY "  >> Invalid First Name."
               ELSE
                   MOVE 0 TO WS-ALPHA-COUNT
                   INSPECT WS-FIRST-NAME TALLYING WS-ALPHA-COUNT
                       FOR ALL WS-LETTERS
                   IF WS-ALPHA-COUNT = 0
                       DISPLAY "  >> Invalid First Name."
                   ELSE
                       MOVE WS-FIRST-NAME TO CUST-FNAME
                       MOVE 'Y' TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM

           MOVE 'N' TO WS-VALID
           PERFORM UNTIL WS-VALID = 'Y'
               DISPLAY "Enter Last Name (1-10 letters): "
               ACCEPT WS-LAST-NAME
               IF WS-LAST-NAME = SPACES
                   DISPLAY "  >> Invalid Last Name."
               ELSE
                   MOVE 0 TO WS-ALPHA-COUNT
                   INSPECT WS-LAST-NAME TALLYING WS-ALPHA-COUNT
                       FOR ALL WS-LETTERS
                   IF WS-ALPHA-COUNT = 0
                       DISPLAY "  >> Invalid Last Name."
                   ELSE
                       MOVE WS-LAST-NAME TO CUST-LNAME
                       MOVE 'Y' TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM

           MOVE 'N' TO WS-VALID
           PERFORM UNTIL WS-VALID = 'Y'
               DISPLAY "Enter Starting Balance:"
               DISPLAY "(0 <= Balance < 100000)"
               ACCEPT WS-BALANCE-STR
               MOVE FUNCTION NUMVAL(WS-BALANCE-STR) TO WS-BALANCE
               IF WS-BALANCE < 0
                   DISPLAY "  >> Invalid Balance."
               ELSE
                   IF WS-BALANCE >= 100000
                       DISPLAY "  >> Invalid Balance."
                   ELSE
                       MOVE WS-BALANCE TO CUST-BALANCE
                       MOVE 'Y' TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM

           WRITE CUSTOMER-RECORD
           DISPLAY "Account " WS-ACCOUNT-ID
           DISPLAY " has been successfully added."

           CLOSE CUSTOMER-FILE
           STOP RUN.
