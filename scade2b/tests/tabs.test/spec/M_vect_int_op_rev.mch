MACHINE M_vect_int_op_rev

OPERATIONS

yy <-- vect_int_op_rev(xx) =
 PRE
   xx : seq(INT)
 THEN
   yy :: { ii | ii : seq(INT) }
 END 
END
