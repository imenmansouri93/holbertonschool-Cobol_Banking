       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETRIEVE-DETAILS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 IDX                     PIC 9(1).
       LINKAGE SECTION.
       01 CUSTOMER-TABLE-LINKED.
           05 CUSTOMER-DATA OCCURS 5 TIMES.
               10 CUST-ID         PIC 9(5).
               10 CUST-FNAME      PIC X(10).
               10 CUST-LNAME      PIC X(10).
               10 CUST-EMAIL      PIC X(25).
               10 CUST-BAL        PIC 9(5)V99.
       01 WS-USER-CHO-LINKED     PIC 9.
       PROCEDURE DIVISION USING CUSTOMER-TABLE-LINKED, WS-USER-CHO-LINKED.
           MOVE WS-USER-CHO-LINKED TO IDX
           DISPLAY " "
           DISPLAY "CUSTOMER DETAILS"
           DISPLAY "----------------"
           DISPLAY "Customer ID   : " CUST-ID(IDX)
           DISPLAY "First Name    : " CUST-FNAME(IDX)
           DISPLAY "Last Name     : " CUST-LNAME(IDX)
           DISPLAY "Email         : " CUST-EMAIL(IDX)
           DISPLAY "Balance       : " CUST-BAL(IDX) "$"
           GOBACK.
