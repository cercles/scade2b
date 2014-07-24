MACHINE M_vect_real_op_slice

OPERATIONS

yy <-- vect_real_op_slice(xx) =
 PRE
   xx : seq(REAL) & dom(xx) = 1 .. 4
 THEN
   yy : ( yy : seq(REAL) & dom(yy) = 1 .. 2 )
 END 
END
