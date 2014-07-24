MACHINE M_vect_int_op_concat

OPERATIONS

yy <-- vect_int_op_concat(xx, zz) =
 PRE
   zz : seq(INT) & dom(zz) = 1 .. 2 &
   xx : seq(INT) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 4 )
 END 
END
