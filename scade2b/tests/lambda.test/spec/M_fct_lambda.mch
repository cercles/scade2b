MACHINE M_fct_lambda

OPERATIONS

rr <-- fct_lambda(aa, bb) =
 PRE
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
