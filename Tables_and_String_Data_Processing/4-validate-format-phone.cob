       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATEPHONE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ORIGINAL-NUMBER PIC X(30).
       01 WS-CLEAN-NUMBER    PIC X(10) VALUE SPACES.
       01 WS-CHAR            PIC X.
       01 WS-INDEX           PIC 9(2) VALUE 1.
       01 WS-POS             PIC 9(2) VALUE 1.
       01 WS-LENGTH          PIC 9(2).

       LINKAGE SECTION.
       01 LK-PHONE-NUMBER    PIC X(30).

       PROCEDURE DIVISION USING LK-PHONE-NUMBER.
       *> Save and trim original input
           MOVE FUNCTION TRIM(LK-PHONE-NUMBER) TO WS-ORIGINAL-NUMBER
           MOVE SPACES TO WS-CLEAN-NUMBER
           MOVE 1 TO WS-POS
           MOVE FUNCTION LENGTH(WS-ORIGINAL-NUMBER) TO WS-LENGTH

       *> Extract only digits
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-LENGTH
               MOVE WS-ORIGINAL-NUMBER(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR NUMERIC
                   IF WS-POS <= 10
                       MOVE WS-CHAR TO WS-CLEAN-NUMBER(WS-POS:1)
                       ADD 1 TO WS-POS
                   END-IF
               END-IF
           END-PERFORM

       *> Check if exactly 10 digits
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-CLEAN-NUMBER)) = 10
               DISPLAY "Formatted Phone: ("
                       WS-CLEAN-NUMBER(1:2) ") "
                       WS-CLEAN-NUMBER(3:4) " "
                       WS-CLEAN-NUMBER(7:4)
           ELSE
               DISPLAY "Invalid phone number: " WS-ORIGINAL-NUMBER
           END-IF

           GOBACK.
