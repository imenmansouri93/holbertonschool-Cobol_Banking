       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACTNAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FULL-NAME    PIC X(50).
       01 WS-FIRST-NAME   PIC X(20).
       01 WS-LAST-NAME    PIC X(20).
       01 WS-TOKEN        PIC X(20) OCCURS 10 TIMES.
       01 WS-TOKEN-COUNT  PIC 9(2).
       01 WS-INDEX        PIC 9(2).

       PROCEDURE DIVISION.

       *> Test case 1
           MOVE FUNCTION TRIM("John Doe") TO WS-FULL-NAME
           PERFORM TOKENIZE-AND-DISPLAY

       *> Test case 2
           MOVE FUNCTION TRIM("Mary Ann Smith") TO WS-FULL-NAME
           PERFORM TOKENIZE-AND-DISPLAY

       *> Test case 3
           MOVE FUNCTION TRIM("Cher") TO WS-FULL-NAME
           PERFORM TOKENIZE-AND-DISPLAY

           STOP RUN.

       *>------------------------------------------------------------------
       TOKENIZE-AND-DISPLAY.
           *> Initialize token table
           MOVE 0 TO WS-TOKEN-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
               MOVE SPACES TO WS-TOKEN(WS-INDEX)
           END-PERFORM

           *> Tokenize using UNSTRING
           UNSTRING WS-FULL-NAME DELIMITED BY SPACE
               INTO WS-TOKEN(1) WS-TOKEN(2) WS-TOKEN(3)
           END-UNSTRING

           *> Count tokens
           MOVE 0 TO WS-TOKEN-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
               IF WS-TOKEN(WS-INDEX) NOT = SPACES
                   ADD 1 TO WS-TOKEN-COUNT
               END-IF
           END-PERFORM

           *> First and last name
           MOVE WS-TOKEN(1) TO WS-FIRST-NAME
           IF WS-TOKEN-COUNT = 1
               MOVE WS-FIRST-NAME TO WS-LAST-NAME
           ELSE
               MOVE WS-TOKEN(WS-TOKEN-COUNT) TO WS-LAST-NAME
           END-IF

           *> Display results
           DISPLAY "First Name: " WS-FIRST-NAME
           DISPLAY "Last Name:  " WS-LAST-NAME
           DISPLAY SPACES

           EXIT.
