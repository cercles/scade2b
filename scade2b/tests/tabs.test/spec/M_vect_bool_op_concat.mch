MACHINE M_vect_bool_op_concat

OPERATIONS

yy <-- vect_bool_op_concat(xx, zz) =
 PRE
   zz : seq(BOOL) & dom(zz) = 1 .. 2 &
   xx : seq(BOOL) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(BOOL) & dom(yy) = 1 .. 4 )
 END 
END
