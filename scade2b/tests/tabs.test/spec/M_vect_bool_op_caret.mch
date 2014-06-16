MACHINE M_vect_bool_op_caret

OPERATIONS

yy <-- vect_bool_op_caret(xx) =
 PRE
   xx : BOOL
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
