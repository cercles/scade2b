MACHINE M_Op_not

OPERATIONS

rr <-- Op_not(aa) =
 PRE
   aa : BOOL
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
