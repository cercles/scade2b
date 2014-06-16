MACHINE M_vect_bool_op_slice

OPERATIONS

yy <-- vect_bool_op_slice(xx) =
 PRE
   xx : seq(BOOL)
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
