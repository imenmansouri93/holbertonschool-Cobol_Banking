IDENTIFICATION DIVISION.
PROGRAM-ID. INITIAL-REPORT.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CUSTOMER-NAME    PIC X(50).
01  WS-BALANCE          PIC X(20).
01  WS-SQL-RESULT       PIC 9(4).

PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY "--- INITIAL BALANCE REPORT ---"
    
    *> Connect to database
    CALL "db_connect" USING WS-SQL-RESULT
    
    IF WS-SQL-RESULT NOT = 0
        DISPLAY "ERROR: Database connection failed"
        STOP RUN
    END-IF
    
    *> Execute the JOIN query
    CALL "db_execute_query" USING 
        "SELECT c.name, a.balance FROM customers c " &
        "JOIN accounts a ON c.customer_id = a.customer_id " &
        "ORDER BY c.customer_id"
        WS-SQL-RESULT
    
    IF WS-SQL-RESULT NOT = 0
        DISPLAY "ERROR: Query execution failed"
        CALL "db_disconnect"
        STOP RUN
    END-IF
    
    *> Fetch and display results
    PERFORM FETCH-AND-DISPLAY UNTIL WS-SQL-RESULT NOT = 0
    
    *> Disconnect
    CALL "db_disconnect"
    
    STOP RUN.

FETCH-AND-DISPLAY.
    CALL "db_fetch_row" USING 
        WS-CUSTOMER-NAME
        WS-BALANCE
        WS-SQL-RESULT
    
    IF WS-SQL-RESULT = 0
        DISPLAY "Customer: " FUNCTION TRIM(WS-CUSTOMER-NAME)
                ", Balance: " FUNCTION TRIM(WS-BALANCE)
    END-IF.
