       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOAN-PAYMENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LOAN-AMOUNT        PIC 9(5).
       01  WS-DURATION-YEARS     PIC 9(2).
       01  WS-INTEREST      PIC 99 VALUE 5.
       01  WS-SIMPLE-INTEREST    PIC 9(5)V99 VALUE 0.
       01  WS-TOTAL-AMOUNT       PIC 9(5)V99 VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "Please enter Loan Amount: "
           ACCEPT WS-LOAN-AMOUNT

           DISPLAY "Please enter Duration in years: "
           ACCEPT WS-DURATION-YEARS

           *> Calcul du simple intérêt
           COMPUTE WS-SIMPLE-INTEREST = (WS-LOAN-AMOUNT * WS-INTEREST
                                         * WS-DURATION-YEARS) / 100

           *> Calcul du montant total à rembourser
           COMPUTE WS-TOTAL-AMOUNT = WS-LOAN-AMOUNT + WS-SIMPLE-INTEREST

           *> Affichage formaté
           DISPLAY " "
           DISPLAY "Loan Amount: " WS-LOAN-AMOUNT "$"
           DISPLAY "Interest Rate: " WS-INTEREST "%"
           DISPLAY "Duration: " WS-DURATION-YEARS " years"
           DISPLAY " "
           DISPLAY "Simple Interest: " WS-SIMPLE-INTEREST "$"
           DISPLAY "Total Amount to be Repaid: " WS-TOTAL-AMOUNT "$"

           STOP RUN.
