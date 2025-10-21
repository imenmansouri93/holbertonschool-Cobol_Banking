       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-SEARCH.
       AUTHOR. HOLBERTON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMERS-FILE ASSIGN TO 'CUSTOMERS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMERS-FILE.
       01 CUSTOMER-REC.
           05 CUST-ID    PIC 9(5).
           05 CUST-NAME  PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-CUST-ID      PIC 9(5).
       01 WS-END-FILE     PIC X VALUE 'N'.
       01 WS-FOUND        PIC X VALUE 'N'.
       01 WS-DISPLAY-LINE PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'Enter Customer ID: ' WITH NO ADVANCING
           ACCEPT WS-CUST-ID

           OPEN INPUT CUSTOMERS-FILE

           PERFORM UNTIL WS-END-FILE = 'Y' OR WS-FOUND = 'Y'
               READ CUSTOMERS-FILE
                   AT END MOVE 'Y' TO WS-END-FILE
               END-READ
               IF WS-END-FILE NOT = 'Y'
                   IF CUST-ID = WS-CUST-ID
                       MOVE 'Y' TO WS-FOUND
                       DISPLAY 'Customer found: ' CUST-NAME
                   END-IF
               END-IF
           END-PERFORM

           IF WS-FOUND NOT = 'Y'
               DISPLAY 'Error: Customer ID not found'
           END-IF

           CLOSE CUSTOMERS-FILE
           STOP RUN.
