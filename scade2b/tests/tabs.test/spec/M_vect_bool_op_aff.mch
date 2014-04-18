MACHINE M_vect_bool_op_aff
SEES M_Enum
OPERATIONS

yy <-- vect_bool_op_aff(xx) =
 PRE
   xx : seq(BOOL)
 THEN
   yy :: { ii | ii : seq(BOOL) }
 END 
END
