       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACTNAMES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FULL-NAME      PIC X(50).
       01 WS-FIRST-NAME     PIC X(20).
       01 WS-LAST-NAME      PIC X(20).
       01 WS-TOKEN          PIC X(20) OCCURS 10 TIMES.
       01 WS-TOKEN-COUNT    PIC 9(2).
       01 WS-INDEX          PIC 9(2).

       LINKAGE SECTION.
       01 LK-FULL-NAME      PIC X(50).

       PROCEDURE DIVISION USING LK-FULL-NAME.

       *>---------------------------------------------------------------
       *>  Extract first and last name from a full name string
       *>---------------------------------------------------------------
           MOVE FUNCTION TRIM(LK-FULL-NAME) TO WS-FULL-NAME

           *> Initialize token list
           MOVE 0 TO WS-TOKEN-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
               MOVE SPACES TO WS-TOKEN(WS-INDEX)
           END-PERFORM

           *> Split the string into tokens separated by space
           UNSTRING WS-FULL-NAME DELIMITED BY ALL SPACES
               INTO WS-TOKEN(1) WS-TOKEN(2) WS-TOKEN(3)
               COUNT IN WS-TOKEN-COUNT
           END-UNSTRING

           *> Count actual tokens (handle multiple spaces)
           MOVE 0 TO WS-TOKEN-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
               IF WS-TOKEN(WS-INDEX) NOT = SPACES
                   ADD 1 TO WS-TOKEN-COUNT
               END-IF
           END-PERFORM

           *> Assign first and last name
           MOVE WS-TOKEN(1) TO WS-FIRST-NAME
           IF WS-TOKEN-COUNT = 1
               MOVE WS-FIRST-NAME TO WS-LAST-NAME
           ELSE
               MOVE WS-TOKEN(WS-TOKEN-COUNT) TO WS-LAST-NAME
           END-IF

           *> Display results (aligned output)
           DISPLAY "First Name: " WS-FIRST-NAME
           DISPLAY "Last Name: " WS-LAST-NAME
           DISPLAY SPACES

           GOBACK.
