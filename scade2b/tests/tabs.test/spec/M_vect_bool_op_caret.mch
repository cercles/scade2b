MACHINE M_vect_bool_op_caret

OPERATIONS

yy <-- vect_bool_op_caret(xx) =
 PRE
   xx : BOOL
 THEN
   yy : ( yy : seq(BOOL) & dom(yy) = 1 .. 2 )
 END 
END
