       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOWERCASE-NAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-NAME       PIC X(20) VALUE SPACES.
       01 LOWERCASE-NAME      PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
           DISPLAY "Enter your name in UPPERCASE: "
           ACCEPT CUSTOMER-NAME

           MOVE CUSTOMER-NAME TO LOWERCASE-NAME

           INSPECT LOWERCASE-NAME
               CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO      "abcdefghijklmnopqrstuvwxyz"

           DISPLAY "Original Name: " CUSTOMER-NAME
           DISPLAY "Lowercase Name: " LOWERCASE-NAME

           STOP RUN.
