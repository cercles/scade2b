MACHINE M_Test_fby2
SEES M_Enum
OPERATIONS

y1, y2 <-- Test_fby2(xx) =
 PRE
   xx : INT
 THEN
   y2 :: { ii | ii : INT }||
   y1 :: { ii | ii : INT }
 END 
END
