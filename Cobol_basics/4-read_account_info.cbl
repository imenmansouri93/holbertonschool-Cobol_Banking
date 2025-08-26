       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-ACCOUNT-INFO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER      PIC 9(6).          *> 6 digits with leading zeros
       01  WS-BALANCE             PIC 9(6)V99.       *> 7 digits before decimal, 2 after

       PROCEDURE DIVISION.
           DISPLAY "Please enter your account number: "
           ACCEPT WS-ACCOUNT-NUMBER

           DISPLAY "Please enter your current balance: "
           ACCEPT WS-BALANCE

           DISPLAY "Account " WS-ACCOUNT-NUMBER
                   " has a balance of : "
                   WS-BALANCE "$"
           STOP RUN.
