       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE ASSIGN TO CUSTMAST
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUST-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CUST-RECORD.
       01 CUST-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       77 WS-EOF        PIC X VALUE 'N'.
       77 WS-COUNT      PIC 9(5) VALUE 0.

       01 WS-CUST-FIELDS.
           05 WS-CUST-ID   PIC X(5).
           05 WS-CUST-NAME PIC X(20).
           05 WS-CUST-ADDR PIC X(55).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "CUSTOMER-REPORTER" 
           ": Starting customer report generation..."
           DISPLAY "CUSTOMER-REPORTER"
           ": ================================"
           OPEN INPUT CUST-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUST-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-COUNT
                       MOVE CUST-RECORD TO WS-CUST-FIELDS
                       DISPLAY "Customer #" WS-COUNT ":"
                       DISPLAY "  ID: " WS-CUST-ID
                       DISPLAY "  Name: " WS-CUST-NAME
                       DISPLAY "  Address: " WS-CUST-ADDR
                       DISPLAY "  ------------------------------"
               END-READ
           END-PERFORM
           CLOSE CUST-FILE
           DISPLAY "CUSTOMER-REPORTER"
           ": ================================"
           DISPLAY "CUSTOMER-REPORTER"
           ": Report generation completed"
           DISPLAY "CUSTOMER-REPORTER"
           ": Total customers processed: " WS-COUNT
           DISPLAY "CUSTOMER-REPORTER"
           ": Report ready for management review"
           STOP RUN.
