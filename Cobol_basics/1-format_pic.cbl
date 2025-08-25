       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-INFO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Customer-ID         PIC X(9)    VALUE "CUST00123".
       01 Account-Balance     PIC 9(5)V99 VALUE 1234.56.
       01 Account-Balance-Fmt PIC 9(5).99.
       01 Interest-Rate       PIC 9V99    VALUE 5.75.
       01 Interest-Rate-Fmt   PIC 9.99.

       PROCEDURE DIVISION.
           MOVE Account-Balance  TO Account-Balance-Fmt.
           MOVE Interest-Rate  TO INTEREST-RATE-FMT.

           DISPLAY "Customer ID : " Customer-ID.
           DISPLAY "Account Balance : " Account-Balance-Fmt.
           DISPLAY "Interest Rate : " Interest-Rate-Fmt "%".

           STOP RUN.
