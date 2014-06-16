MACHINE M_Test_call

OPERATIONS

rr <-- Test_call(xx) =
 PRE
   xx : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
