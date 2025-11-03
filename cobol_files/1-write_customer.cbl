       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE-CUSTOMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD       PIC X(33).
       01  CUSTOMER-FIELDS REDEFINES CUSTOMER-RECORD.
           05  CUST-ID-FIELD     PIC X(5).
           05  CUST-FNAME-FIELD  PIC X(10).
           05  CUST-LNAME-FIELD  PIC X(10).
           05  CUST-BAL-FIELD    PIC X(8).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS           PIC XX.
       01  WS-CUST-ID            PIC X(5).
       01  WS-CUST-FNAME         PIC X(10).
       01  WS-CUST-LNAME         PIC X(10).
       01  WS-CUST-BALANCE       PIC 9(5)V99.
       01  WS-BAL-DISPLAY        PIC 00000.99.

       PROCEDURE DIVISION.
       BEGIN.
           OPEN OUTPUT CUSTOMER-FILE

           DISPLAY "Enter Customer ID (5 digits): "
           ACCEPT WS-CUST-ID
           DISPLAY "Enter First Name (max 10 characters): "
           ACCEPT WS-CUST-FNAME
           DISPLAY "Enter Last Name (max 10 characters): "
           ACCEPT WS-CUST-LNAME
           DISPLAY "Enter Balance (less than 99999.99): "
           ACCEPT WS-CUST-BALANCE

           *> Préparer les champs du fichier
           MOVE WS-CUST-ID TO CUST-ID-FIELD
           MOVE WS-CUST-FNAME TO CUST-FNAME-FIELD
           MOVE WS-CUST-LNAME TO CUST-LNAME-FIELD
           MOVE WS-CUST-BALANCE TO WS-BAL-DISPLAY
           MOVE WS-BAL-DISPLAY TO CUST-BAL-FIELD

           *> Écrire le nouveau client dans le fichier
           WRITE CUSTOMER-RECORD

           DISPLAY "Customer record added successfully!"

           CLOSE CUSTOMER-FILE
           STOP RUN.
