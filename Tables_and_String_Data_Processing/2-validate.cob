       IDENTIFICATION DIVISION.
       PROGRAM-ID. ValidateIBAN.

       DATA DIVISION.
       LINKAGE SECTION.

       01 IBAN-LINKED        PIC X(50).

       LOCAL-STORAGE SECTION.
       77 IBAN-CLEAN         PIC X(50).
       77 IBAN-LEN           PIC 9(2).
       77 VALID              PIC X VALUE 'Y'.
       77 I                  PIC 9(2).
       77 CHAR               PIC X.

       PROCEDURE DIVISION USING IBAN-LINKED.

           *> Trim spaces
           UNSTRING IBAN-LINKED DELIMITED BY SPACE INTO IBAN-CLEAN

           *> Compute length
           INSPECT IBAN-CLEAN TALLYING IBAN-LEN FOR CHARACTERS

           *> Validate length
           IF IBAN-LEN NOT = 22
               MOVE 'N' TO VALID
           END-IF

           *> Validate country code
           IF VALID = 'Y'
               IF IBAN-CLEAN(1:2) NOT = "GB"
                   MOVE 'N' TO VALID
               END-IF
           END-IF

           *> Validate check digits (positions 3-4)
           IF VALID = 'Y'
               PERFORM VARYING I FROM 3 BY 1 UNTIL I > 4
                   IF IBAN-CLEAN(I:1) < '0' OR IBAN-CLEAN(I:1) > '9'
                       MOVE 'N' TO VALID
                   END-IF
               END-PERFORM
           END-IF

           *> Validate bank code (positions 5-8, letters)
           IF VALID = 'Y'
               PERFORM VARYING I FROM 5 BY 1 UNTIL I > 8
                   IF IBAN-CLEAN(I:1) < 'A' OR IBAN-CLEAN(I:1) > 'Z'
                       MOVE 'N' TO VALID
                   END-IF
               END-PERFORM
           END-IF

           *> Validate sort code (positions 9-14, digits)
           IF VALID = 'Y'
               PERFORM VARYING I FROM 9 BY 1 UNTIL I > 14
                   IF IBAN-CLEAN(I:1) < '0' OR IBAN-CLEAN(I:1) > '9'
                       MOVE 'N' TO VALID
                   END-IF
               END-PERFORM
           END-IF

           *> Validate account number (positions 15-22, digits)
           IF VALID = 'Y'
               PERFORM VARYING I FROM 15 BY 1 UNTIL I > 22
                   IF IBAN-CLEAN(I:1) < '0' OR IBAN-CLEAN(I:1) > '9'
                       MOVE 'N' TO VALID
                   END-IF
               END-PERFORM
           END-IF

           *> Display result
           IF VALID = 'Y'
               DISPLAY "IBAN is valid: " IBAN-CLEAN
           ELSE
               DISPLAY "IBAN is invalid: " IBAN-CLEAN
           END-IF

           GOBACK.
