       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DISC-PRICE   PIC 9(4)V99.
       01 WS-TOTAL        PIC 9(6)V99 VALUE 0.
       01 WS-ANSWER       PIC X.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-ANSWER = 'N'
               CALL 'PROCESS-ORDER' USING WS-DISC-PRICE
               ADD WS-DISC-PRICE TO WS-TOTAL
               DISPLAY "Another item? (Y/N): " WITH NO ADVANCING
               ACCEPT WS-ANSWER
               IF WS-ANSWER = 'Y' OR WS-ANSWER = 'y'
                   CONTINUE
               ELSE
                   MOVE 'N' TO WS-ANSWER
               END-IF
           END-PERFORM

           *> Display total amount in fixed format
           DISPLAY "Total amount: " 
               FUNCTION NUMVAL-C (WS-TOTAL) UPON CONSOLE
           STOP RUN.
