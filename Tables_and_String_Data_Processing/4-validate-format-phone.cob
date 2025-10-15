       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATEPHONE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RAW-PHONE       PIC X(30).
       01 WS-CLEAN-PHONE     PIC 9(10) VALUE ZEROS.
       01 WS-DIGIT           PIC X.
       01 WS-INDEX           PIC 9(2) VALUE 1.
       01 WS-CLEAN-INDEX     PIC 9(2) VALUE 1.
       01 WS-LENGTH          PIC 9(2).

       LINKAGE SECTION.
       01 LK-PHONE           PIC X(30).

       PROCEDURE DIVISION USING LK-PHONE.

           MOVE LK-PHONE TO WS-RAW-PHONE

           MOVE 1 TO WS-INDEX
           MOVE 1 TO WS-CLEAN-INDEX

           INSPECT WS-RAW-PHONE TALLYING WS-LENGTH FOR CHARACTERS

           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-LENGTH
               MOVE WS-RAW-PHONE(WS-INDEX:1) TO WS-DIGIT
               IF WS-DIGIT >= "0" AND WS-DIGIT <= "9"
                   MOVE WS-DIGIT TO WS-CLEAN-PHONE(WS-CLEAN-INDEX:1)
                   ADD 1 TO WS-CLEAN-INDEX
               END-IF
           END-PERFORM

           SUBTRACT 1 FROM WS-CLEAN-INDEX

           IF WS-CLEAN-INDEX NOT = 10
               DISPLAY "Invalid phone number: " WS-RAW-PHONE
           ELSE
               DISPLAY "Formatted Phone: (" WS-CLEAN-PHONE(1:2) ") "
                       WS-CLEAN-PHONE(3:4) " " WS-CLEAN-PHONE(7:4)
           END-IF

           GOBACK.
