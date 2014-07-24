MACHINE M_link_assume

OPERATIONS

rr <-- link_assume(aa, cc, dd, bb) =
 PRE
   dd : INT &
   cc : INT &
   bb : BOOL & (bool(not (bb = TRUE)) = TRUE) &
   aa : INT & ((-aa) > 1)
 THEN
   rr : ( rr : BOOL )
 END 
END
