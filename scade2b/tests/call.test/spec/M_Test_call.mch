MACHINE M_Test_call
SEES M_Enum
OPERATIONS

rr <-- Test_call(xx) =
 PRE
   xx : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
