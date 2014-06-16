MACHINE M_vect_real_op_def

OPERATIONS

yy <-- vect_real_op_def(xx, zz) =
 PRE
   zz : REAL &
   xx : REAL
 THEN
   yy :: { ii | ii : seq(REAL) }
 END 
END
