MACHINE M_vect_int_op_caret
SEES M_Enum

OPERATIONS

yy <-- vect_int_op_caret(xx) =
 PRE
   xx : INT
 THEN
   yy :: { ii | ii : seq(INT) }
 END 
END
