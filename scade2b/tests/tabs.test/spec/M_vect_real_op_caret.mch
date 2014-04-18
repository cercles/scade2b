MACHINE M_vect_real_op_caret
SEES M_Enum
OPERATIONS

yy <-- vect_real_op_caret(xx) =
 PRE
   xx : REAL
 THEN
   yy :: { ii | ii : seq(REAL) }
 END 
END
