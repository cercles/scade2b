MACHINE M_Test_const
SEES M_Consts
OPERATIONS

rr <-- Test_const(xx) =
 PRE
   xx : INT
 THEN
   rr : ( rr : BOOL )
 END 
END
