MACHINE M_Err_mach
OPERATIONS

rr <-- Err_mach(aa, bb, cc, dd) =
 PRE
   dd : seq(seq(INT)) &
   cc : seq(INT) &
   bb : REAL &
   aa : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
