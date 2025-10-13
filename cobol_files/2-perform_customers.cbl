       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM-CUSTOMERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD       PIC X(33).

       WORKING-STORAGE SECTION.
       01 WS-CUST-ID-INPUT      PIC X(5).
       01 WS-CUST-FNAME         PIC X(10).
       01 WS-CUST-LNAME         PIC X(10).
       01 WS-BALANCE-INPUT      PIC X(8).
       01 WS-ANSWER             PIC X VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN EXTEND CUSTOMER-FILE
           PERFORM UNTIL WS-ANSWER = "N" OR WS-ANSWER = "n"
               DISPLAY "Enter Customer ID (5 digits): "
               ACCEPT WS-CUST-ID-INPUT
               DISPLAY "Enter First Name (max 10 chars): "
               ACCEPT WS-CUST-FNAME
               DISPLAY "Enter Last Name (max 10 chars): "
               ACCEPT WS-CUST-LNAME
               DISPLAY "Enter Balance (less than 99999.99): "
               ACCEPT WS-BALANCE-INPUT

               MOVE SPACES TO CUSTOMER-RECORD
               MOVE WS-CUST-ID-INPUT TO CUSTOMER-RECORD(1:5)
               MOVE WS-CUST-FNAME TO CUSTOMER-RECORD(6:10)
               MOVE WS-CUST-LNAME TO CUSTOMER-RECORD(16:10)
               MOVE WS-BALANCE-INPUT TO CUSTOMER-RECORD(26:8)

               WRITE CUSTOMER-RECORD
               DISPLAY "Record written. Add another? (Y/N): "
               ACCEPT WS-ANSWER
           END-PERFORM
           CLOSE CUSTOMER-FILE
           DISPLAY "All records saved. Goodbye!"
           STOP RUN.
