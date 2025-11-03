       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMATDATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ORIGINAL-DATE        PIC X(20).
       01 WS-CLEAN-DATE           PIC X(8) VALUE SPACES.
       01 WS-CHAR                 PIC X(1).
       01 WS-INDEX                PIC 9(2) VALUE 1.
       01 WS-POS                  PIC 9(2) VALUE 1.
       01 WS-LENGTH               PIC 9(2) VALUE 0.
       01 WS-FORMATTED-DATE       PIC X(10).

       LINKAGE SECTION.
       01 LK-DATE                 PIC X(20).

       PROCEDURE DIVISION USING LK-DATE.

           *> Stocker l'input original
           MOVE LK-DATE TO WS-ORIGINAL-DATE
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-ORIGINAL-DATE))
                TO WS-LENGTH

           MOVE SPACES TO WS-CLEAN-DATE
           MOVE 1 TO WS-POS

           *> Extraire uniquement les chiffres
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-LENGTH
               MOVE WS-ORIGINAL-DATE(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR NUMERIC
                   IF WS-POS <= 8
                       MOVE WS-CHAR TO WS-CLEAN-DATE(WS-POS:1)
                       ADD 1 TO WS-POS
                   END-IF
               END-IF
           END-PERFORM

           *> Vérifier que l'on a exactement 8 chiffres
           IF WS-POS = 9
               *> Construire le format MM/DD/YYYY
               MOVE SPACES TO WS-FORMATTED-DATE
               MOVE WS-CLEAN-DATE(5:2) TO WS-FORMATTED-DATE(1:2)  *> Mois
               MOVE "/" TO WS-FORMATTED-DATE(3:1)
               MOVE WS-CLEAN-DATE(7:2) TO WS-FORMATTED-DATE(4:2)  *> Jour
               MOVE "/" TO WS-FORMATTED-DATE(6:1)
               MOVE WS-CLEAN-DATE(1:4) TO WS-FORMATTED-DATE(7:4)  *> Année

               DISPLAY "Formatted Date: " WS-FORMATTED-DATE
           ELSE
               DISPLAY "Invalid date: " WS-ORIGINAL-DATE
           END-IF

           GOBACK.
