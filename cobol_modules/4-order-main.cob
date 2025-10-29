       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MORE            PIC X VALUE "Y".
       01 WS-DISC-PRICE      PIC 9(4)V99 VALUE 0.
       01 WS-TOTAL           PIC 9(6)V99 VALUE 0.
       01 WS-DISP-TOTAL      PIC ZZZZZ9.99.   *> <-- Ajouté un Z de plus pour l'alignement

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-MORE NOT = "Y"
               CALL 'PROCESS-ORDER' USING WS-DISC-PRICE
               ADD WS-DISC-PRICE TO WS-TOTAL

               DISPLAY "Another item? (Y/N): " WITH NO ADVANCING
               ACCEPT WS-MORE
               MOVE FUNCTION UPPER-CASE(WS-MORE) TO WS-MORE
           END-PERFORM

           MOVE WS-TOTAL TO WS-DISP-TOTAL
           DISPLAY "Total amount: " WS-DISP-TOTAL

           STOP RUN.
