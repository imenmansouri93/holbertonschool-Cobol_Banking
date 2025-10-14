       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-SLIP.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LNK-NAME       PIC X(20).
       01  LNK-BASIC      PIC 9(5)V99.
       01  LNK-ALLOW      PIC 9(4)V99.
       01  LNK-DEDUCT     PIC 9(4)V99.
       01  LNK-GROSS      PIC 9(6)V99.
       01  LNK-NET        PIC 9(6)V99.
       PROCEDURE DIVISION
       USING LNK-NAME, LNK-BASIC, LNK-ALLOW, LNK-DEDUCT,
                             LNK-GROSS, LNK-NET.
           DISPLAY "==========================="
           DISPLAY "       SALARY SLIP"
           DISPLAY "---------------------------"
           DISPLAY "Employee Name : " LNK-NAME
           DISPLAY "Basic Salary  : " LNK-BASIC 
           DISPLAY "Allowance     : " LNK-ALLOW
           DISPLAY "Deductions    : " LNK-DEDUCT
           DISPLAY "Gross Salary  : " LNK-GROSS
           DISPLAY "Net Salary    : " LNK-NET
           DISPLAY "==========================="
           EXIT PROGRAM.
