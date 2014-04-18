MACHINE M_vect_real_op_index
SEES M_Enum
OPERATIONS

yy <-- vect_real_op_index(xx) =
 PRE
   xx : seq(REAL)
 THEN
   yy :: { ii | ii : REAL }
 END 
END
