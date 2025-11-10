       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITIAL-REPORT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME            PIC X(50).
       01  WS-BALANCE         PIC 9(7)V99.
       01  WS-EOF             PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           DISPLAY "--- INITIAL BALANCE REPORT ---".

           CALL 'pg_connect' USING
               BY CONTENT "dbname=schooldb user=postgres"
           END-CALL.

           PERFORM UNTIL WS-EOF = 'Y'
              CALL 'pg_fetch_customer_balance'
                   USING WS-NAME WS-BALANCE WS-EOF
              END-CALL

              IF WS-EOF NOT = 'Y'
                 DISPLAY "Customer: " WS-NAME ", Balance: " WS-BALANCE
              END-IF
           END-PERFORM.

           CALL 'pg_disconnect'
           END-CALL.

           DISPLAY "--- End of Task 1 ---".
           STOP RUN.
