MACHINE M_vect_real_op_slice
SEES M_Enum

OPERATIONS

yy <-- vect_real_op_slice(xx) =
 PRE
   xx : seq(REAL)
 THEN
   yy :: { ii | ii : seq(REAL) }
 END 
END
