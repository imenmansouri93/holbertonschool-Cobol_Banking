       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMATDATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ORIGINAL-DATE    PIC X(20).
       01 WS-CLEAN-DATE       PIC X(8) VALUE SPACES.
       01 WS-CHAR             PIC X.
       01 WS-INDEX            PIC 9(2) VALUE 1.
       01 WS-POS              PIC 9(2) VALUE 1.
       01 WS-LENGTH           PIC 9(2).

       LINKAGE SECTION.
       01 LK-DATE-INPUT       PIC X(20).

       PROCEDURE DIVISION USING LK-DATE-INPUT.

           *> Copier et nettoyer l'entrée
           MOVE FUNCTION TRIM(LK-DATE-INPUT) TO WS-ORIGINAL-DATE
           MOVE SPACES TO WS-CLEAN-DATE
           MOVE 1 TO WS-POS
           MOVE FUNCTION LENGTH(WS-ORIGINAL-DATE) TO WS-LENGTH

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

           *> Vérifier si 8 chiffres extraits
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-CLEAN-DATE)) = 8
               *> Afficher au format MM/DD/YYYY avec espaces finaux
               DISPLAY "Formatted Date: " 
                       WS-CLEAN-DATE(5:2) "/" 
                       WS-CLEAN-DATE(7:2) "/" 
                       WS-CLEAN-DATE(1:4)
           ELSE
               DISPLAY "Invalid date: " LK-DATE-INPUT
           END-IF

           GOBACK.
