       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-VALIDATION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SEARCH-TYPE           PIC X.
       01  CUSTOMER-ID           PIC X(10).
       01  CUSTOMER-NAME         PIC X(20).
       01  I                     PIC 99.
       01  CHAR                  PIC X.
       01  INVALID-FLAG          PIC X VALUE 'N'.
       01  INVALID-REASON        PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Search Type (I=ID, N=Name): "
           ACCEPT SEARCH-TYPE
           IF SEARCH-TYPE = 'I'
               PERFORM VALIDATE-ID
           ELSE
               IF SEARCH-TYPE = 'N'
                   PERFORM VALIDATE-NAME
               ELSE
                   DISPLAY "Invalid search type. Use I or N."
               END-IF
           END-IF
           STOP RUN.

       VALIDATE-ID.
           DISPLAY "Enter Customer ID: "
           ACCEPT CUSTOMER-ID
           MOVE 'N' TO INVALID-FLAG
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE CUSTOMER-ID(I:1) TO CHAR
               IF CHAR = SPACE
                   EXIT PERFORM
               END-IF
               IF CHAR < '0' OR CHAR > '9'
                   MOVE 'Y' TO INVALID-FLAG
                   STRING "Invalid character detected: Non-numeric ID"
                          DELIMITED BY SIZE
                          INTO INVALID-REASON
                   END-STRING
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF INVALID-FLAG = 'Y'
               DISPLAY INVALID-REASON
           ELSE
               DISPLAY "Input validation passed!"
           END-IF
           .

       VALIDATE-NAME.
           DISPLAY "Enter Customer Name: "
           ACCEPT CUSTOMER-NAME
           MOVE 'N' TO INVALID-FLAG
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               MOVE CUSTOMER-NAME(I:1) TO CHAR
               IF CHAR = SPACE
                   CONTINUE
               ELSE
                   IF CHAR = "'" OR CHAR = ";" OR CHAR = "-"
                       MOVE 'Y' TO INVALID-FLAG
                       STRING "Invalid character detected: "
                              DELIMITED BY SIZE
                              CHAR DELIMITED BY SIZE
                              INTO INVALID-REASON
                       END-STRING
                       EXIT PERFORM
                   ELSE
                       IF (CHAR < 'A' OR (CHAR > 'Z' AND CHAR < 'a')
                           OR CHAR > 'z')
                           AND CHAR NOT = SPACE
                           MOVE 'Y' TO INVALID-FLAG
                           MOVE "Invalid name input" TO INVALID-REASON
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF INVALID-FLAG = 'Y'
               DISPLAY INVALID-REASON
           ELSE
               DISPLAY "Input validation passed!"
           END-IF
           .
