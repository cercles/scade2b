MACHINE M_vect_int_op_rev

OPERATIONS

yy <-- vect_int_op_rev(xx) =
 PRE
   xx : seq(INT) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 2 )
 END 
END
