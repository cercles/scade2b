MACHINE M_vect_int_op_slice

OPERATIONS

yy <-- vect_int_op_slice(xx) =
 PRE
   xx : seq(INT) & dom(xx) = 1 .. 4
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 2 )
 END 
END
