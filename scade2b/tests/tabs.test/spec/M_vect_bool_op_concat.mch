MACHINE M_vect_bool_op_concat
SEES M_Enum

OPERATIONS

yy <-- vect_bool_op_concat(xx, zz) =
 PRE
   zz : seq(BOOL) &
   xx : seq(BOOL)
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
