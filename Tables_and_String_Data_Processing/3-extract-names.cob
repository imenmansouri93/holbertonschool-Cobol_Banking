       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACTNAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FULL-NAME    PIC X(50).
       01 WS-FIRST-NAME   PIC X(20).
       01 WS-LAST-NAME    PIC X(20).
       01 WS-REST         PIC X(50).

       LINKAGE SECTION.
       01 LK-FULL-NAME    PIC X(50).

       PROCEDURE DIVISION USING LK-FULL-NAME.
       *> Copy full name (with spaces) to working storage
           MOVE LK-FULL-NAME TO WS-FULL-NAME

       *> Trim leading and trailing spaces for tokenizing
           MOVE FUNCTION TRIM(WS-FULL-NAME) TO WS-REST

       *> Get first word
           UNSTRING WS-REST
               DELIMITED BY SPACE
               INTO WS-FIRST-NAME WS-REST
           END-UNSTRING

       *> Determine last name
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-REST)) = 0
               *> Only one word -> last name = first name
               MOVE WS-FIRST-NAME TO WS-LAST-NAME
           ELSE
               *> Otherwise, last token = last word
               UNSTRING WS-REST
                   DELIMITED BY ALL SPACES
                   INTO WS-LAST-NAME
               END-UNSTRING
           END-IF

       *> Display results with spaces preserved
           DISPLAY "First Name: " WS-FIRST-NAME
           DISPLAY "Last Name:  " WS-LAST-NAME

           GOBACK.
