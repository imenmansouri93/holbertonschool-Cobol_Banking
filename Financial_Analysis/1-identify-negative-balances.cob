       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDENTIFYNEGATIVE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BALANCE-FILE ASSIGN TO 'balances.idx'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BALANCE-FILE.
       01  BALANCE-RECORD.
           05 CUSTOMER-ID        PIC X(7).
           05 FILLER             PIC X(3).
           05 BALANCE-AMOUNT     PIC S9(7)V99.

       WORKING-STORAGE SECTION.
       01 EOF-FLAG               PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           OPEN INPUT BALANCE-FILE

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BALANCE-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF BALANCE-AMOUNT < 0
                           DISPLAY "Account " CUSTOMER-ID
                                   "   - Balance: " BALANCE-AMOUNT
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BALANCE-FILE
           STOP RUN.
