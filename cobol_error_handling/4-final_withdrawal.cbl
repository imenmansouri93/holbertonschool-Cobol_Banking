*****************************************************************
      * FINAL-WITHDRAWAL - Banking Withdrawal System with Security
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL-WITHDRAWAL.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS ACC-STATUS.
           SELECT TEMP-FILE ASSIGN TO "TEMP.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           SELECT AUTH-FILE ASSIGN TO "AUTHORIZED_USERS.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS AUTH-STATUS.
           SELECT ERROR-LOG ASSIGN TO "WITHDRAWAL_ERRORS.LOG"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LOG-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE
           RECORD CONTAINS 34 CHARACTERS.
       01  ACCOUNT-RECORD.
           05 ACC-ID               PIC 9(6).
           05 ACC-NAME             PIC X(20).
           05 ACC-BALANCE          PIC 9(5)V99.
       
       FD  TEMP-FILE
           RECORD CONTAINS 34 CHARACTERS.
       01  TEMP-RECORD.
           05 TEMP-ID              PIC 9(6).
           05 TEMP-NAME            PIC X(20).
           05 TEMP-BALANCE         PIC 9(5)V99.
       
       FD  AUTH-FILE
           RECORD CONTAINS 6 CHARACTERS.
       01  AUTH-RECORD.
           05 AUTH-ID              PIC 9(6).
       
       FD  ERROR-LOG.
       01  LOG-RECORD              PIC X(200).
       
       WORKING-STORAGE SECTION.
       01  ACC-STATUS              PIC XX.
       01  AUTH-STATUS             PIC XX.
       01  LOG-STATUS              PIC XX.
       01  WS-ACCOUNT-ID           PIC 9(6).
       01  WS-WITHDRAWAL           PIC 9(5)V99.
       01  WS-NEW-BALANCE          PIC 9(5)V99.
       01  WS-FOUND                PIC X VALUE 'N'.
       01  WS-AUTHORIZED           PIC X VALUE 'N'.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-AUTH-EOF             PIC X VALUE 'N'.
       01  WS-DISPLAY-BALANCE      PIC ZZ,ZZ9.99.
       
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
       01  WS-INPUT-AMT            PIC X(10).
       01  WS-VALID-AMT            PIC X VALUE 'Y'.
       01  WS-TEMP-NUM             PIC 9(5)V99.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter Account ID: ".
           ACCEPT WS-ACCOUNT-ID.
           DISPLAY "Enter Withdrawal Amount: ".
           ACCEPT WS-INPUT-AMT.
           
           PERFORM VALIDATE-INPUT.
           IF WS-VALID-AMT = 'Y'
               PERFORM CHECK-ACCOUNT-EXISTS
               IF WS-FOUND = 'Y'
                   PERFORM CHECK-AUTHORIZATION
                   IF WS-AUTHORIZED = 'Y'
                       PERFORM PROCESS-WITHDRAWAL
                   END-IF
               END-IF
           END-IF.
           
           STOP RUN.
       
       VALIDATE-INPUT.
           MOVE 'Y' TO WS-VALID-AMT.
           
           IF FUNCTION TEST-NUMVAL(WS-INPUT-AMT) = 0
               COMPUTE WS-WITHDRAWAL = FUNCTION NUMVAL(WS-INPUT-AMT)
                   ON SIZE ERROR
                       MOVE 'N' TO WS-VALID-AMT
                       DISPLAY "Error: Invalid amount."
                       PERFORM LOG-INVALID-AMOUNT
               END-COMPUTE
           ELSE
               MOVE 'N' TO WS-VALID-AMT
               DISPLAY "Error: Invalid amount."
               PERFORM LOG-INVALID-AMOUNT
           END-IF.
       
       CHECK-ACCOUNT-EXISTS.
           MOVE 'N' TO WS-FOUND.
           MOVE 'N' TO WS-EOF.
           
           OPEN INPUT ACCOUNTS-FILE.
           IF ACC-STATUS NOT = '00'
               DISPLAY "Error opening ACCOUNTS.DAT"
               STOP RUN
           END-IF.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ACC-ID = WS-ACCOUNT-ID
                           MOVE 'Y' TO WS-FOUND
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNTS-FILE.
           
           IF WS-FOUND = 'N'
               DISPLAY "Error: Account ID not found."
               PERFORM LOG-NOT-FOUND
           END-IF.
       
       CHECK-AUTHORIZATION.
           MOVE 'N' TO WS-AUTHORIZED.
           MOVE 'N' TO WS-AUTH-EOF.
           
           OPEN INPUT AUTH-FILE.
           IF AUTH-STATUS NOT = '00'
               DISPLAY "Error: Cannot open AUTHORIZED_USERS.DAT"
               STOP RUN
           END-IF.
           
           PERFORM UNTIL WS-AUTH-EOF = 'Y'
               READ AUTH-FILE
                   AT END
                       MOVE 'Y' TO WS-AUTH-EOF
                   NOT AT END
                       IF AUTH-ID = WS-ACCOUNT-ID
                           MOVE 'Y' TO WS-AUTHORIZED
                           MOVE 'Y' TO WS-AUTH-EOF
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE AUTH-FILE.
           
           IF WS-AUTHORIZED = 'N'
               DISPLAY "Error: Unauthorized access."
               PERFORM LOG-UNAUTHORIZED
           END-IF.
       
       PROCESS-WITHDRAWAL.
           MOVE 'N' TO WS-EOF.
           
           OPEN INPUT ACCOUNTS-FILE.
           IF ACC-STATUS NOT = '00'
               DISPLAY "Error opening ACCOUNTS.DAT"
               STOP RUN
           END-IF.
           
           OPEN OUTPUT TEMP-FILE.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ACC-ID = WS-ACCOUNT-ID
                           PERFORM EXECUTE-WITHDRAWAL
                       ELSE
                           WRITE TEMP-RECORD FROM ACCOUNT-RECORD
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNTS-FILE.
           CLOSE TEMP-FILE.
           
           PERFORM REPLACE-ORIGINAL-FILE.
       
       EXECUTE-WITHDRAWAL.
           IF ACC-BALANCE < WS-WITHDRAWAL
               DISPLAY "Error: Insufficient funds."
               PERFORM LOG-INSUFFICIENT-FUNDS
               WRITE TEMP-RECORD FROM ACCOUNT-RECORD
           ELSE
               SUBTRACT WS-WITHDRAWAL FROM ACC-BALANCE 
                   GIVING WS-NEW-BALANCE
                   ON SIZE ERROR
                       DISPLAY "Error: Calculation error."
                       WRITE TEMP-RECORD FROM ACCOUNT-RECORD
                   NOT ON SIZE ERROR
                       MOVE WS-NEW-BALANCE TO ACC-BALANCE
                       MOVE WS-NEW-BALANCE TO WS-DISPLAY-BALANCE
                       DISPLAY "New balance for "
                           FUNCTION TRIM(ACC-NAME) ": "
                           WS-DISPLAY-BALANCE
                       WRITE TEMP-RECORD FROM ACCOUNT-RECORD
               END-SUBTRACT
           END-IF.
       
       REPLACE-ORIGINAL-FILE.
           MOVE "rm ACCOUNTS.DAT" TO WS-CMD.
           CALL "SYSTEM" USING WS-CMD.
           MOVE "mv TEMP.DAT ACCOUNTS.DAT" TO WS-CMD.
           CALL "SYSTEM" USING WS-CMD.
       
       LOG-INVALID-AMOUNT.
           PERFORM GET-TIMESTAMP.
           STRING WS-TIMESTAMP DELIMITED BY SIZE
               "  - ERROR: Invalid withdrawal amount entered"
               DELIMITED BY SIZE
               INTO WS-ERROR-MSG
           END-STRING.
           PERFORM WRITE-TO-LOG.
       
       LOG-UNAUTHORIZED.
           PERFORM GET-TIMESTAMP.
           STRING WS-TIMESTAMP DELIMITED BY SIZE
               "  - ERROR: Unauthorized access attempt for "
               "Account ID " DELIMITED BY SIZE
               WS-ACCOUNT-ID DELIMITED BY SIZE
               INTO WS-ERROR-MSG
           END-STRING.
           PERFORM WRITE-TO-LOG.
       
       LOG-NOT-FOUND.
           PERFORM GET-TIMESTAMP.
           STRING WS-TIMESTAMP DELIMITED BY SIZE
               "  - ERROR: Account ID " DELIMITED BY SIZE
               WS-ACCOUNT-ID DELIMITED BY SIZE
               " not found in ACCOUNTS.DAT" DELIMITED BY SIZE
               INTO WS-ERROR-MSG
           END-STRING.
           PERFORM WRITE-TO-LOG.
       
       LOG-INSUFFICIENT-FUNDS.
           PERFORM GET-TIMESTAMP.
           STRING WS-TIMESTAMP DELIMITED BY SIZE
               "  - ERROR: Insufficient funds for Account ID "
               DELIMITED BY SIZE
               WS-ACCOUNT-ID DELIMITED BY SIZE
               INTO WS-ERROR-MSG
           END-STRING.
           PERFORM WRITE-TO-LOG.
       
       WRITE-TO-LOG.
           OPEN EXTEND ERROR-LOG.
           IF LOG-STATUS = '00' OR LOG-STATUS = '05'
               WRITE LOG-RECORD FROM WS-ERROR-MSG
               CLOSE ERROR-LOG
           ELSE
               DISPLAY "Warning: Could not write to log"
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
