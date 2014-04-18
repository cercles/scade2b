MACHINE M_vect_bool_op_index
SEES M_Enum
OPERATIONS

yy <-- vect_bool_op_index(xx) =
 PRE
   xx : seq(BOOL)
 THEN
   yy :: { ii | ii : BOOL }
 END 
END
