       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTION-HISTORY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-CUSTID          PIC X(9).
       77 WS-COUNT           PIC 9 VALUE 5.
       77 I                  PIC 9.
       77 WS-RUNNING-BALANCE PIC S9(7)V99 VALUE 0.
       77 WS-AMOUNT-DISPLAY  PIC S9(7)V99.
       77 WS-BALANCE-DISPLAY PIC S9(7)V99.
       77 WS-TEMP-DATE       PIC X(10).
       77 WS-TEMP-DESC       PIC X(20).
       77 WS-TEMP-AMT        PIC S9(7)V99.

       01 TRANS-TAB OCCURS 5 TIMES.
          05 T-DATE   PIC X(10).
          05 T-DESC   PIC X(20).
          05 T-AMT    PIC S9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Customer ID (9 characters):" WITH NO ADVANCING.
           ACCEPT WS-CUSTID.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               DISPLAY "Enter transaction ", I, " date (DD/MM/YYYY): " 
                       WITH NO ADVANCING
               ACCEPT WS-TEMP-DATE
               MOVE WS-TEMP-DATE TO T-DATE OF TRANS-TAB(I)

               DISPLAY "Enter transaction ", I, " description: " 
                       WITH NO ADVANCING
               ACCEPT WS-TEMP-DESC
               MOVE WS-TEMP-DESC TO T-DESC OF TRANS-TAB(I)

               DISPLAY "Enter transaction ", I, " amount: " 
                       WITH NO ADVANCING
               ACCEPT WS-TEMP-AMT
               MOVE WS-TEMP-AMT TO T-AMT OF TRANS-TAB(I)
           END-PERFORM.


           DISPLAY "--------------------------------------------------".
           DISPLAY "      CUSTOMER TRANSACTION HISTORY".
           DISPLAY "      Customer ID : " WS-CUSTID.
           DISPLAY "--------------------------------------------------".
           DISPLAY "Date       Description          Amount     Balance".
           DISPLAY "--------------------------------------------------".

           MOVE 0 TO WS-RUNNING-BALANCE.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-COUNT
               ADD T-AMT OF TRANS-TAB(I) TO WS-RUNNING-BALANCE
               MOVE T-AMT OF TRANS-TAB(I) TO WS-AMOUNT-DISPLAY
               MOVE WS-RUNNING-BALANCE TO WS-BALANCE-DISPLAY

               DISPLAY T-DATE OF TRANS-TAB(I), " ",
                       T-DESC OF TRANS-TAB(I), " ",
                       WS-AMOUNT-DISPLAY, " ",
                       WS-BALANCE-DISPLAY
           END-PERFORM.

           DISPLAY "--------------------------------------------------".
           DISPLAY "End of Report".

           STOP RUN.
