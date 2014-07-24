MACHINE M_vect_real_op_def

OPERATIONS

yy <-- vect_real_op_def(xx, zz) =
 PRE
   zz : REAL &
   xx : REAL
 THEN
   yy : ( yy : seq(REAL) & dom(yy) = 1 .. 2 )
 END 
END
