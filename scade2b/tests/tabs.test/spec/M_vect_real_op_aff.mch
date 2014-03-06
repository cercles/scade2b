MACHINE M_vect_real_op_aff
SEES M_Enum

OPERATIONS

yy <-- vect_real_op_aff(xx) =
 PRE
   xx : seq(REAL)
 THEN
   yy :: { ii | ii : seq(REAL) }
 END 
END
