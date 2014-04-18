MACHINE M_vect_int_op_slice
SEES M_Enum
OPERATIONS

yy <-- vect_int_op_slice(xx) =
 PRE
   xx : seq(INT)
 THEN
   yy :: { ii | ii : seq(INT) }
 END 
END
