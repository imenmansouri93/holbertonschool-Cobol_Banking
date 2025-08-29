       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE-TRANSACTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-AMOUNT      PIC 9(5)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           
           ACCEPT WS-AMOUNT FROM COMMAND-LINE.

           IF WS-AMOUNT > 0 AND WS-AMOUNT <= 10000
               DISPLAY "Transaction is valid."
           ELSE
               DISPLAY "Invalid transaction amount."
           END-IF.

           STOP RUN.
