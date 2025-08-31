       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROL-FREAK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BALANCE           PIC S9(6)V99 VALUE 1000.00.
       01 DISPLAY-BALANCE   PIC Z,ZZZ.99.
       01 OPERATION         PIC X VALUE SPACE.
       01 AMOUNT            PIC S9(6)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL OPERATION = "Q"
               DISPLAY "Enter operation [D=Deposit, W=Withdraw,"
               DISPLAY " B=Balance, Q=Quit]: "
               ACCEPT OPERATION

               EVALUATE OPERATION
                   WHEN "D"
                       DISPLAY "Enter deposit amount: "
                       ACCEPT AMOUNT
                       ADD AMOUNT TO BALANCE
                       DISPLAY "Deposit successful."
                   WHEN "W"
                       DISPLAY "Enter withdrawal amount: "
                       ACCEPT AMOUNT
                       IF BALANCE >= AMOUNT
                           SUBTRACT AMOUNT FROM BALANCE
                           DISPLAY "Withdrawal successful."
                       ELSE
                           DISPLAY "Insufficient funds."
                       END-IF
                   WHEN "B"
                       MOVE BALANCE TO DISPLAY-BALANCE
                       DISPLAY "Current Balance: " DISPLAY-BALANCE
                   WHEN "Q"
                       DISPLAY "Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid option. Please try again."
               END-EVALUATE

               DISPLAY SPACE
           END-PERFORM

           STOP RUN.
