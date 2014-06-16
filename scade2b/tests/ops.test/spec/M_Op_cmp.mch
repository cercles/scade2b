MACHINE M_Op_cmp

OPERATIONS

rr <-- Op_cmp(aa, bb, cc, dd, ee, ff) =
 PRE
   ff : INT &
   ee : INT &
   dd : INT &
   cc : INT &
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
