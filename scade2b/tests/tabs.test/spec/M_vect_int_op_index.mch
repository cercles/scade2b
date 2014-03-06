MACHINE M_vect_int_op_index
SEES M_Enum

OPERATIONS

yy <-- vect_int_op_index(xx) =
 PRE
   xx : seq(INT)
 THEN
   yy :: { ii | ii : INT }
 END 
END
