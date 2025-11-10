IDENTIFICATION DIVISION.
PROGRAM-ID. INITIAL-REPORT.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  DB-HANDLE           USAGE POINTER.
01  STMT-HANDLE         USAGE POINTER.
01  DB-CONNINFO         PIC X(200) VALUE 
    "host=localhost dbname=banking_db user=banking_user password=banking_pass".
01  SQL-QUERY           PIC X(200) VALUE
    "SELECT c.name, a.balance FROM customers c JOIN accounts a ON c.customer_id = a.customer_id ORDER BY c.customer_id".
01  WS-CUSTOMER-NAME    PIC X(50).
01  WS-BALANCE          PIC X(20).
01  WS-UNUSED           PIC X(1).
01  WS-FETCH-RESULT     PIC S9(4) COMP.
01  WS-RC               PIC S9(4) COMP VALUE 0.

PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY "--- INITIAL BALANCE REPORT ---"
    
    *> Connect to database
    CALL "DB_CONNECT" USING 
        BY CONTENT DB-CONNINFO
        BY REFERENCE DB-HANDLE
    
    IF DB-HANDLE = NULL
        DISPLAY "ERROR: Database connection failed"
        STOP RUN
    END-IF
    
    *> Execute the JOIN query
    CALL "DB_QUERY" USING
        BY REFERENCE DB-HANDLE
        BY CONTENT SQL-QUERY
        BY REFERENCE STMT-HANDLE
    
    IF STMT-HANDLE = NULL
        DISPLAY "ERROR: Query execution failed"
        CALL "DB_DISCONNECT" USING 
            BY REFERENCE DB-HANDLE
            BY REFERENCE WS-RC
        STOP RUN
    END-IF
    
    *> Fetch and display all rows
    PERFORM FETCH-AND-DISPLAY UNTIL WS-FETCH-RESULT NOT = 0
    
    *> Disconnect from database
    CALL "DB_DISCONNECT" USING
        BY REFERENCE DB-HANDLE
        BY REFERENCE WS-RC
    
    STOP RUN.

FETCH-AND-DISPLAY.
    CALL "DB_FETCH" USING
        BY REFERENCE STMT-HANDLE
        BY REFERENCE WS-CUSTOMER-NAME
        BY REFERENCE WS-BALANCE
        BY REFERENCE WS-UNUSED
        RETURNING WS-FETCH-RESULT
    
    IF WS-FETCH-RESULT = 0
        DISPLAY "Customer: " FUNCTION TRIM(WS-CUSTOMER-NAME)
                ", Balance: " FUNCTION TRIM(WS-BALANCE)
    END-IF.
    