MACHINE M_vect_bool_op_slice
SEES M_Enum

OPERATIONS

yy <-- vect_bool_op_slice(xx) =
 PRE
   xx : seq(BOOL)
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
