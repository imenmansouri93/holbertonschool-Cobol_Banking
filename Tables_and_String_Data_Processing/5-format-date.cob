       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMATDATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CLEANED-DATE       PIC X(8) VALUE SPACES.
       01 CHR                PIC X.
       01 POS-IN             PIC 9(2) VALUE 1.
       01 POS-OUT            PIC 9(2) VALUE 1.
       01 IS-VALID           PIC X VALUE "Y".
       01 COUNT-DIGITS       PIC 9 VALUE 0.
       01 YYYY               PIC 9(4).
       01 MM                 PIC 9(2).
       01 DD                 PIC 9(2).
       01 FORMATTED-DATE     PIC X(10).
       01 WS-LENGTH          PIC 9(2).

       LINKAGE SECTION.
       01 RAW-DATE           PIC X(20).

       PROCEDURE DIVISION USING RAW-DATE.

       *> Trim spaces and get actual length
           MOVE FUNCTION TRIM(RAW-DATE) TO CLEANED-DATE
           MOVE FUNCTION LENGTH(FUNCTION TRIM(RAW-DATE)) TO WS-LENGTH
           MOVE 1 TO POS-IN
           MOVE 1 TO POS-OUT
           MOVE 0 TO COUNT-DIGITS
           MOVE "Y" TO IS-VALID

       *> Extract digits
           PERFORM VARYING POS-IN FROM 1 BY 1 UNTIL POS-IN > WS-LENGTH
               MOVE RAW-DATE(POS-IN:1) TO CHR
               IF CHR >= "0" AND CHR <= "9"
                   IF POS-OUT <= 8
                       MOVE CHR TO CLEANED-DATE(POS-OUT:1)
                       ADD 1 TO POS-OUT
                       ADD 1 TO COUNT-DIGITS
                   ELSE
                       MOVE "N" TO IS-VALID
                   END-IF
               END-IF
           END-PERFORM

       *> Check for exactly 8 digits
           IF COUNT-DIGITS NOT = 8
               MOVE "N" TO IS-VALID
           END-IF

       *> Extract year, month, day if valid
           IF IS-VALID = "Y"
               MOVE CLEANED-DATE(1:4) TO YYYY
               MOVE CLEANED-DATE(5:2) TO MM
               MOVE CLEANED-DATE(7:2) TO DD
               IF MM < 1 OR MM > 12
                   MOVE "N" TO IS-VALID
               END-IF
               IF DD < 1 OR DD > 31
                   MOVE "N" TO IS-VALID
               END-IF
           END-IF

       *> Output result
           IF IS-VALID = "Y"
               STRING
                   MM DELIMITED BY SIZE
                   "/" DELIMITED BY SIZE
                   DD DELIMITED BY SIZE
                   "/" DELIMITED BY SIZE
                   YYYY DELIMITED BY SIZE
                   INTO FORMATTED-DATE
               END-STRING
               DISPLAY "Formatted Date: " FORMATTED-DATE
           ELSE
               DISPLAY "Invalid date: " RAW-DATE
           END-IF

           GOBACK.
