MACHINE M_vect_bool_op_rev

OPERATIONS

yy <-- vect_bool_op_rev(xx) =
 PRE
   xx : seq(BOOL) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(BOOL) & dom(yy) = 1 .. 2 )
 END 
END
