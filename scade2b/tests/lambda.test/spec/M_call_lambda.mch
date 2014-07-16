MACHINE M_call_lambda

OPERATIONS

rr <-- call_lambda(aa) =
 PRE
   aa : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
