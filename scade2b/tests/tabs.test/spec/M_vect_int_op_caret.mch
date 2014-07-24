MACHINE M_vect_int_op_caret

OPERATIONS

yy <-- vect_int_op_caret(xx) =
 PRE
   xx : INT
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 2 )
 END 
END
