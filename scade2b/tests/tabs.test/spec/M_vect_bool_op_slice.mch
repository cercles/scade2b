MACHINE M_vect_bool_op_slice

OPERATIONS

yy <-- vect_bool_op_slice(xx) =
 PRE
   xx : seq(BOOL) & dom(xx) = 1 .. 4
 THEN
   yy : ( yy : seq(BOOL) & dom(yy) = 1 .. 2 )
 END 
END
