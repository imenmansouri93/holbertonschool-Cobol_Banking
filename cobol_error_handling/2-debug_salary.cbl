       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEBUG-SALARY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID-TEXT       PIC X(5).
           05 EMP-NAME          PIC X(20).
           05 EMP-SALARY-TEXT   PIC X(8).

       WORKING-STORAGE SECTION.
       01 WS-INPUT-ID           PIC X(5).
       01 WS-INPUT-BONUS-TEXT   PIC X(10).
       01 WS-BONUS-NUM          PIC 9(5)V99 VALUE 0.
       01 WS-SALARY-NUM         PIC 9(5)V99 VALUE 0.
       01 WS-NEW-SALARY         PIC 9(5)V99 VALUE 0.
       01 FILE-STATUS           PIC XX.
       01 WS-FOUND-FLAG         PIC X VALUE 'N'.
       01 WS-END-FLAG           PIC X VALUE 'N'.
       01 WS-NEW-SALARY-DSP     PIC Z(5)9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter Employee ID: "
           ACCEPT WS-INPUT-ID

           DISPLAY "[DEBUG] Searching for Employee ID: "
           DISPLAY WS-INPUT-ID

           OPEN INPUT EMPLOYEE-FILE

           PERFORM UNTIL WS-FOUND-FLAG = 'Y' OR WS-END-FLAG = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-END-FLAG
                   NOT AT END
                       IF EMP-ID-TEXT = WS-INPUT-ID
                           MOVE 'Y' TO WS-FOUND-FLAG

                           COMPUTE WS-SALARY-NUM = 
                               FUNCTION NUMVAL(EMP-SALARY-TEXT)
                           MOVE WS-SALARY-NUM TO WS-NEW-SALARY-DSP

                           DISPLAY "[DEBUG] Found: "
                           DISPLAY EMP-NAME " with salary "
                                   WS-NEW-SALARY-DSP

                           DISPLAY "Enter Bonus Amount: "
                           ACCEPT WS-INPUT-BONUS-TEXT

                           DISPLAY "[DEBUG] Bonus entered: "
                           DISPLAY WS-INPUT-BONUS-TEXT

                           COMPUTE WS-BONUS-NUM = 
                               FUNCTION NUMVAL(WS-INPUT-BONUS-TEXT)
                           COMPUTE WS-NEW-SALARY = WS-SALARY-NUM 
                               + WS-BONUS-NUM
                           MOVE WS-NEW-SALARY TO WS-NEW-SALARY-DSP

                           DISPLAY "[DEBUG] New calculated salary: "
                           DISPLAY WS-NEW-SALARY-DSP
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND-FLAG = 'Y'
               DISPLAY "Updated Salary for " EMP-NAME ": " WS-NEW-SALARY-DSP
           ELSE
               DISPLAY "Error: Employee ID not found."
           END-IF

           CLOSE EMPLOYEE-FILE
           STOP RUN.
