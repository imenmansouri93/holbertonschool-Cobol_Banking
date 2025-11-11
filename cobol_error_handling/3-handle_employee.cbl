*****************************************************************
      * HANDLE-EMPLOYEE - Batch Salary Processing with Error Logging
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HANDLE-EMPLOYEE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEES.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS EMP-STATUS.
           SELECT TEMP-FILE ASSIGN TO "TEMP.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           SELECT ERROR-LOG ASSIGN TO "ERRORS.LOG"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LOG-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           RECORD CONTAINS 33 CHARACTERS.
       01  EMPLOYEE-RECORD.
           05 EMP-ID               PIC 9(5).
           05 EMP-NAME             PIC X(20).
           05 EMP-SALARY           PIC 9(6)V99.
       
       FD  TEMP-FILE
           RECORD CONTAINS 33 CHARACTERS.
       01  TEMP-RECORD.
           05 TEMP-ID              PIC 9(5).
           05 TEMP-NAME            PIC X(20).
           05 TEMP-SALARY          PIC 9(6)V99.
       
       FD  ERROR-LOG.
       01  LOG-RECORD              PIC X(200).
       
       WORKING-STORAGE SECTION.
       01  EMP-STATUS              PIC XX.
       01  LOG-STATUS              PIC XX.
       01  WS-EMPLOYEE-ID          PIC 9(5).
       01  WS-BONUS                PIC 9(3)V99.
       01  WS-NEW-SALARY           PIC 9(6)V99.
       01  WS-FOUND                PIC X VALUE 'N'.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-OVERFLOW             PIC X VALUE 'N'.
       01  WS-DISPLAY-SALARY       PIC ZZZ,ZZ9.99.
       
       01  WS-TIMESTAMP.
           05 WS-YEAR              PIC 9(4).
           05 FILLER               PIC X VALUE '-'.
           05 WS-MONTH             PIC 99.
           05 FILLER               PIC X VALUE '-'.
           05 WS-DAY               PIC 99.
           05 FILLER               PIC X VALUE ' '.
           05 WS-HOUR              PIC 99.
           05 FILLER               PIC X VALUE ':'.
           05 WS-MINUTE            PIC 99.
           05 FILLER               PIC X VALUE ':'.
           05 WS-SECOND            PIC 99.
       
       01  WS-DATE-NUM             PIC 9(8).
       01  WS-TIME-NUM             PIC 9(6).
       01  WS-ERROR-MSG            PIC X(200).
       01  WS-CMD                  PIC X(50).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter Employee ID: ".
           ACCEPT WS-EMPLOYEE-ID.
           DISPLAY "[DEBUG] Searching for Employee ID: " 
               WS-EMPLOYEE-ID.
           
           PERFORM SEARCH-AND-UPDATE.
           STOP RUN.
       
       SEARCH-AND-UPDATE.
           MOVE 'N' TO WS-FOUND.
           MOVE 'N' TO WS-EOF.
           MOVE 'N' TO WS-OVERFLOW.
           
           OPEN INPUT EMPLOYEE-FILE.
           IF EMP-STATUS NOT = '00'
               DISPLAY "Error opening EMPLOYEES.DAT"
               STOP RUN
           END-IF.
           
           OPEN OUTPUT TEMP-FILE.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-ID = WS-EMPLOYEE-ID
                           MOVE 'Y' TO WS-FOUND
                           DISPLAY "[DEBUG] Found: " 
                               FUNCTION TRIM(EMP-NAME)
                               " with salary " EMP-SALARY
                           DISPLAY "Enter Bonus Amount: "
                           ACCEPT WS-BONUS
                           PERFORM UPDATE-SALARY
                       ELSE
                           WRITE TEMP-RECORD FROM EMPLOYEE-RECORD
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE.
           CLOSE TEMP-FILE.
           
           IF WS-FOUND = 'N'
               DISPLAY "[DEBUG] Employee ID not found: " 
                   WS-EMPLOYEE-ID
               DISPLAY "Error: Employee ID not found."
               PERFORM LOG-NOT-FOUND-ERROR
               MOVE "rm TEMP.DAT" TO WS-CMD
               CALL "SYSTEM" USING WS-CMD
           ELSE
               IF WS-OVERFLOW = 'N'
                   PERFORM REPLACE-ORIGINAL-FILE
               ELSE
                   MOVE "rm TEMP.DAT" TO WS-CMD
                   CALL "SYSTEM" USING WS-CMD
               END-IF
           END-IF.
       
       UPDATE-SALARY.
           ADD WS-BONUS TO EMP-SALARY GIVING WS-NEW-SALARY
               ON SIZE ERROR
                   MOVE 'Y' TO WS-OVERFLOW
                   DISPLAY "Error: Bonus too large. Salary update "
                       "failed due to overflow."
                   PERFORM LOG-OVERFLOW-ERROR
                   WRITE TEMP-RECORD FROM EMPLOYEE-RECORD
               NOT ON SIZE ERROR
                   MOVE WS-NEW-SALARY TO EMP-SALARY
                   MOVE WS-NEW-SALARY TO WS-DISPLAY-SALARY
                   DISPLAY "Updated Salary for " 
                       FUNCTION TRIM(EMP-NAME) ": " 
                       WS-DISPLAY-SALARY
                   WRITE TEMP-RECORD FROM EMPLOYEE-RECORD
           END-ADD.
       
       REPLACE-ORIGINAL-FILE.
           MOVE "rm EMPLOYEES.DAT" TO WS-CMD.
           CALL "SYSTEM" USING WS-CMD.
           MOVE "mv TEMP.DAT EMPLOYEES.DAT" TO WS-CMD.
           CALL "SYSTEM" USING WS-CMD.
       
       LOG-OVERFLOW-ERROR.
           PERFORM GET-TIMESTAMP.
           STRING WS-TIMESTAMP DELIMITED BY SIZE
               "  - ERROR: Bonus too large for Employee ID "
               DELIMITED BY SIZE
               WS-EMPLOYEE-ID DELIMITED BY SIZE
               ". Salary update failed due to overflow."
               DELIMITED BY SIZE
               INTO WS-ERROR-MSG
           END-STRING.
           PERFORM WRITE-TO-LOG.
       
       LOG-NOT-FOUND-ERROR.
           PERFORM GET-TIMESTAMP.
           STRING WS-TIMESTAMP DELIMITED BY SIZE
               "  - ERROR: Employee ID " DELIMITED BY SIZE
               WS-EMPLOYEE-ID DELIMITED BY SIZE
               " not found in EMPLOYEES.DAT." DELIMITED BY SIZE
               INTO WS-ERROR-MSG
           END-STRING.
           PERFORM WRITE-TO-LOG.
       
       WRITE-TO-LOG.
           OPEN EXTEND ERROR-LOG.
           IF LOG-STATUS = '00' OR LOG-STATUS = '05'
               WRITE LOG-RECORD FROM WS-ERROR-MSG
               CLOSE ERROR-LOG
           ELSE
               DISPLAY "Warning: Could not write to ERRORS.LOG"
           END-IF.
       
       GET-TIMESTAMP.
           ACCEPT WS-DATE-NUM FROM DATE YYYYMMDD.
           ACCEPT WS-TIME-NUM FROM TIME.
           
           MOVE WS-DATE-NUM(1:4) TO WS-YEAR.
           MOVE WS-DATE-NUM(5:2) TO WS-MONTH.
           MOVE WS-DATE-NUM(7:2) TO WS-DAY.
           MOVE WS-TIME-NUM(1:2) TO WS-HOUR.
           MOVE WS-TIME-NUM(3:2) TO WS-MINUTE.
           MOVE WS-TIME-NUM(5:2) TO WS-SECOND.
    