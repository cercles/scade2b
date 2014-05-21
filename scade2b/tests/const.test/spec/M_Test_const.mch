MACHINE M_Test_const
SEES M_Consts, M_Enum
OPERATIONS

rr <-- Test_const(xx) =
 PRE
   xx : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
