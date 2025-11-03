       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMATDATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CLEANED-DATE       PIC X(8) VALUE SPACES.
       01 CHR                PIC X.
       01 POS-IN             PIC 9(2) VALUE 1.
       01 POS-OUT            PIC 9(2) VALUE 1.
       01 COUNT-DIGITS       PIC 9 VALUE 0.
       01 YYYY               PIC 9(4).
       01 MM                 PIC 9(2).
       01 DD                 PIC 9(2).
       01 FORMATTED-DATE     PIC X(10).

       LINKAGE SECTION.
       01 RAW-DATE           PIC X(20).

       PROCEDURE DIVISION USING RAW-DATE.

           *> Extraire uniquement les chiffres
           MOVE SPACES TO CLEANED-DATE
           MOVE 1 TO POS-IN
           MOVE 1 TO POS-OUT
           MOVE 0 TO COUNT-DIGITS

           PERFORM VARYING POS-IN FROM 1 BY 1 UNTIL POS-IN > 20
               MOVE RAW-DATE(POS-IN:1) TO CHR
               IF CHR >= "0" AND CHR <= "9"
                   IF POS-OUT <= 8
                       MOVE CHR TO CLEANED-DATE(POS-OUT:1)
                       ADD 1 TO POS-OUT
                       ADD 1 TO COUNT-DIGITS
                   END-IF
               END-IF
           END-PERFORM

           *> Vérifier que l'on a exactement 8 chiffres
           IF COUNT-DIGITS = 8
               MOVE CLEANED-DATE(7:2) TO DD
               MOVE CLEANED-DATE(5:2) TO MM
               MOVE CLEANED-DATE(1:4) TO YYYY

               STRING
                   DD DELIMITED BY SIZE
                   "/" DELIMITED BY SIZE
                   MM DELIMITED BY SIZE
                   "/" DELIMITED BY SIZE
                   YYYY DELIMITED BY SIZE
                   INTO FORMATTED-DATE
               END-STRING

               DISPLAY "Formatted Date: " FORMATTED-DATE
           ELSE
               DISPLAY "Invalid date: " RAW-DATE
           END-IF

           GOBACK.
