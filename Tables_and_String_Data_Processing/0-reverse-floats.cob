       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-FLOATS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Table of 100 floats
       01  FLOAT-TABLE.
           05  FLOAT-ITEM       PIC 9(4)V99 OCCURS 100 TIMES.

       *> Variables for input and display
       01  WS-INPUT-STR         PIC X(10).
       01  WS-DISP-FLOAT        PIC 0(4).99.
       01  WS-DISP-NUM          PIC 999.
       01  IDX                  PIC 9(3) VALUE 1.
       01  IDX-REV              PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PARA.

           *> Read 100 values
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 100
               DISPLAY "Enter float #" WITH NO ADVANCING
               MOVE IDX TO WS-DISP-NUM
               DISPLAY WS-DISP-NUM ": " WITH NO ADVANCING
               ACCEPT WS-INPUT-STR
               COMPUTE FLOAT-ITEM(IDX) = FUNCTION NUMVAL(WS-INPUT-STR)
           END-PERFORM

           DISPLAY ""
           DISPLAY "Reversed Values:"

           *> Display in reverse order
           PERFORM VARYING IDX-REV FROM 100 BY -1 UNTIL IDX-REV < 1
               MOVE FLOAT-ITEM(IDX-REV) TO WS-DISP-FLOAT
               MOVE IDX-REV TO WS-DISP-NUM
               DISPLAY "Float " WS-DISP-NUM "#: " WS-DISP-FLOAT
           END-PERFORM

           STOP RUN.
