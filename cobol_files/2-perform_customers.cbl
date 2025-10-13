       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM-CUSTOMERS-APPEND.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD       PIC X(33).

       WORKING-STORAGE SECTION.
       01 WS-CUST-ID            PIC 9(5).
       01 WS-CUST-FNAME         PIC X(10).
       01 WS-CUST-LNAME         PIC X(10).
       01 WS-BALANCE            PIC 9(5)V99.
       01 WS-BALANCE-STR        PIC X(8).
       01 WS-ANSWER             PIC X VALUE "Y".

       PROCEDURE DIVISION.
       BEGIN.
           OPEN EXTEND CUSTOMER-FILE
           PERFORM UNTIL WS-ANSWER NOT = "Y"
               DISPLAY "Enter Customer ID (5 digits): "
               ACCEPT WS-CUST-ID
               DISPLAY "Enter First Name (max 10 characters): "
               ACCEPT WS-CUST-FNAME
               DISPLAY "Enter Last Name (max 10 characters): "
               ACCEPT WS-CUST-LNAME
               DISPLAY "Enter Balance (less than 99999.99): "
               ACCEPT WS-BALANCE

               MOVE WS-BALANCE TO WS-BALANCE-STR

               STRING
                   WS-CUST-ID DELIMITED BY SIZE
                   WS-CUST-FNAME DELIMITED BY SIZE
                   WS-CUST-LNAME DELIMITED BY SIZE
                   WS-BALANCE-STR DELIMITED BY SIZE
               INTO CUSTOMER-RECORD

               WRITE CUSTOMER-RECORD

               DISPLAY "Record written. Add another? (Y/N): "
               ACCEPT WS-ANSWER
               MOVE FUNCTION UPPER-CASE(WS-ANSWER) TO WS-ANSWER
           END-PERFORM
           CLOSE CUSTOMER-FILE
           DISPLAY "All records saved. Goodbye!"
           STOP RUN.
