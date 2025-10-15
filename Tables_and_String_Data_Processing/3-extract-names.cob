       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACTNAMES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST-NAME      PIC X(50).
       01 WS-LAST-NAME       PIC X(50).
       01 WS-TEMP-NAME       PIC X(50).
       01 WS-WORDS.
           05 WS-WORD OCCURS 10 TIMES PIC X(50).

       LINKAGE SECTION.
       01 LK-FULL-NAME       PIC X(50).

       PROCEDURE DIVISION USING LK-FULL-NAME.

           *> Trim spaces
           MOVE FUNCTION TRIM(LK-FULL-NAME) TO WS-TEMP-NAME

           *> Split into words (UNSTRING gère tous les mots d'un coup)
           UNSTRING WS-TEMP-NAME
               DELIMITED BY SPACE
               INTO WS-WORD(1)
                    WS-WORD(2)
                    WS-WORD(3)
                    WS-WORD(4)
                    WS-WORD(5)
                    WS-WORD(6)
                    WS-WORD(7)
                    WS-WORD(8)
                    WS-WORD(9)
                    WS-WORD(10)
           END-UNSTRING

           *> Trouve le dernier mot
           IF WS-WORD(10) NOT = SPACES
               MOVE WS-WORD(10) TO WS-LAST-NAME
           ELSE IF WS-WORD(9) NOT = SPACES
               MOVE WS-WORD(9) TO WS-LAST-NAME
           ELSE IF WS-WORD(8) NOT = SPACES
               MOVE WS-WORD(8) TO WS-LAST-NAME
           ELSE IF WS-WORD(7) NOT = SPACES
               MOVE WS-WORD(7) TO WS-LAST-NAME
           ELSE IF WS-WORD(6) NOT = SPACES
               MOVE WS-WORD(6) TO WS-LAST-NAME
           ELSE IF WS-WORD(5) NOT = SPACES
               MOVE WS-WORD(5) TO WS-LAST-NAME
           ELSE IF WS-WORD(4) NOT = SPACES
               MOVE WS-WORD(4) TO WS-LAST-NAME
           ELSE IF WS-WORD(3) NOT = SPACES
               MOVE WS-WORD(3) TO WS-LAST-NAME
           ELSE IF WS-WORD(2) NOT = SPACES
               MOVE WS-WORD(2) TO WS-LAST-NAME
           ELSE
               MOVE WS-WORD(1) TO WS-LAST-NAME
           END-IF

           *> Premier mot = première case
           MOVE WS-WORD(1) TO WS-FIRST-NAME

           *> Display results
           DISPLAY "First Name: " WS-FIRST-NAME
           DISPLAY "Last Name: "  WS-LAST-NAME

           GOBACK.
