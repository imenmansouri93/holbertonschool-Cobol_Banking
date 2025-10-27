       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERFLOW-HANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEES ASSIGN TO "EMPLOYEES.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEES.
       01  EMPLOYEE-RECORD.
           05  EMP-ID             PIC 9(5).
           05  EMP-NAME           PIC X(20).
           05  EMP-SALARY         PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       77  WS-FILE-STATUS         PIC XX.
       77  WS-SEARCH-ID           PIC 9(5).
       77  WS-BONUS               PIC 9(5)V99.
       77  WS-FOUND               PIC X VALUE 'N'.
       77  WS-NEW-SALARY          PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           DISPLAY "Enter Bonus Amount: " WITH NO ADVANCING
           ACCEPT WS-BONUS

           OPEN INPUT EMPLOYEES
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ EMPLOYEES
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       IF EMP-ID = WS-SEARCH-ID
                           MOVE "Y" TO WS-FOUND
                           PERFORM UPDATE-SALARY
                           MOVE "10" TO WS-FILE-STATUS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEES

           IF WS-FOUND NOT = "Y"
               DISPLAY "Error: Employee ID not found."
           END-IF
           STOP RUN.

       UPDATE-SALARY.
           COMPUTE WS-NEW-SALARY = EMP-SALARY + WS-BONUS
               ON SIZE ERROR
                   DISPLAY "Error: Bonus too large. Salary update failed "
      -            "due to overflow."
               NOT ON SIZE ERROR
                   DISPLAY "Updated Salary for " EMP-NAME ": "
      -            "$" WS-NEW-SALARY
           END-COMPUTE.
