MACHINE M_Test_guarantee

OPERATIONS

rr <-- Test_guarantee(xx, yy) =
 PRE
   yy : INT &
   xx : INT
 THEN
   rr : ( rr : BOOL & (rr <= 100) )
 END 
END
