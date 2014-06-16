MACHINE M_Op_unops

OPERATIONS

rr <-- Op_unops(aa) =
 PRE
   aa : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
