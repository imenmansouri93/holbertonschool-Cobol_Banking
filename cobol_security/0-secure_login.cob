       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECURE-LOGIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USERS-FILE.
       01  USERS-RECORD.
           05 USER-ID-FLD        PIC X(8).
           05 USER-NAME-FLD      PIC X(20).
           05 PASSWORD-FLD       PIC X(8).
           05 ACCESS-LEVEL-FLD   PIC 9.

       WORKING-STORAGE SECTION.
       77 WS-USER-ID             PIC X(8).
       77 WS-PASSWORD            PIC X(8).
       77 WS-FOUND               PIC X VALUE "N".
       77 WS-FILE-STATUS         PIC XX.
       77 WS-END-FILE            PIC X VALUE "N".

       77 WS-DISPLAY-USER-NAME   PIC X(20).
       77 WS-DISPLAY-ACCESS      PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "BASIC BANKING LOGIN SYSTEM"
           DISPLAY "=========================="

           *> Get User Input
           PERFORM GET-USER-CREDENTIALS

           *> Open File
           OPEN INPUT USERS-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error: USERS.DAT file not found."
               STOP RUN
           END-IF

           *> Search for User ID
           PERFORM UNTIL WS-END-FILE = "Y" OR WS-FOUND = "Y"
               READ USERS-FILE
                   AT END
                       MOVE "Y" TO WS-END-FILE
                   NOT AT END
                       IF WS-USER-ID = USER-ID-FLD
                           MOVE "Y" TO WS-FOUND
                           MOVE USER-NAME-FLD TO WS-DISPLAY-USER-NAME
                           MOVE ACCESS-LEVEL-FLD TO WS-DISPLAY-ACCESS
                       END-IF
               END-READ
           END-PERFORM

           CLOSE USERS-FILE

           *> Evaluate result
           IF WS-FOUND = "Y"
               IF WS-PASSWORD = PASSWORD-FLD
                   DISPLAY " "
                   DISPLAY "Welcome, " WS-DISPLAY-USER-NAME
                           ". Access level: " WS-DISPLAY-ACCESS
               ELSE
                   DISPLAY " "
                   DISPLAY "Invalid password. Authentication failed."
               END-IF
           ELSE
               DISPLAY " "
               DISPLAY "User not found."
           END-IF

           STOP RUN.

       GET-USER-CREDENTIALS.
           DISPLAY "Enter User ID: " WITH NO ADVANCING
           ACCEPT WS-USER-ID
           DISPLAY "Enter Password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           IF WS-USER-ID = SPACES OR WS-PASSWORD = SPACES
               DISPLAY "Error: User ID and Password cannot be empty."
               STOP RUN
           END-IF.
