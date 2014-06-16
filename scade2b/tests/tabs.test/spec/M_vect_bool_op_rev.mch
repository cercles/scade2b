MACHINE M_vect_bool_op_rev

OPERATIONS

yy <-- vect_bool_op_rev(xx) =
 PRE
   xx : seq(BOOL)
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
