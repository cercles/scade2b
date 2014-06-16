MACHINE M_vect_real_op_rev

OPERATIONS

yy <-- vect_real_op_rev(xx) =
 PRE
   xx : seq(REAL)
 THEN
   yy :: { ii | ii : seq(REAL) }
 END 
END
