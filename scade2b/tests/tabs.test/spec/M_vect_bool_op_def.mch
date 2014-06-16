MACHINE M_vect_bool_op_def

OPERATIONS

yy <-- vect_bool_op_def(xx, zz) =
 PRE
   zz : BOOL &
   xx : BOOL
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
