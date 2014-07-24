MACHINE M_vect_real_op_aff

OPERATIONS

yy <-- vect_real_op_aff(xx) =
 PRE
   xx : seq(REAL) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(REAL) & dom(yy) = 1 .. 2 )
 END 
END
