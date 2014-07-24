MACHINE M_Op_cast

OPERATIONS

rr <-- Op_cast(xx, yy) =
 PRE
   yy : REAL &
   xx : INT
 THEN
   rr : ( rr : INT )
 END 
END
