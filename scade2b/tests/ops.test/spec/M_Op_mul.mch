MACHINE M_Op_mul

OPERATIONS

rr <-- Op_mul(aa, bb) =
 PRE
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
