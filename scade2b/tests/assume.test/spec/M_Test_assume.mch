MACHINE M_Test_assume

OPERATIONS

rr <-- Test_assume(xx, yy) =
 PRE
   yy : INT & (yy <= 100) &
   xx : INT & (xx <= 100)
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
