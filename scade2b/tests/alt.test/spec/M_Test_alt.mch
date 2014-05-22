MACHINE M_Test_alt
SEES M_Enum
OPERATIONS

rr <-- Test_alt(xx, yy, bb) =
 PRE
   bb : BOOL &
   yy : INT &
   xx : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
