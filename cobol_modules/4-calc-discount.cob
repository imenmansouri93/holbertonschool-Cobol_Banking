       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-DISCOUNT.

       DATA DIVISION.
       LINKAGE SECTION.
       01 LNK-PRICE        PIC 9(4)V99.
       01 LNK-CODE         PIC X.
       01 LNK-DISC-PRICE   PIC 9(4)V99.

       PROCEDURE DIVISION USING LNK-PRICE, LNK-CODE, LNK-DISC-PRICE.
           EVALUATE LNK-CODE
               WHEN 'A' 
                   COMPUTE LNK-DISC-PRICE = LNK-PRICE * 0.9
               WHEN 'B'
                   COMPUTE LNK-DISC-PRICE = LNK-PRICE * 0.8
               WHEN OTHER
                   MOVE LNK-PRICE TO LNK-DISC-PRICE
           END-EVALUATE
           GOBACK.
