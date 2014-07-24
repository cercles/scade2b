MACHINE M_vect_real_op_concat

OPERATIONS

yy <-- vect_real_op_concat(xx, zz) =
 PRE
   zz : seq(REAL) & dom(zz) = 1 .. 2 &
   xx : seq(REAL) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(REAL) & dom(yy) = 1 .. 4 )
 END 
END
