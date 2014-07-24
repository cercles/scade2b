MACHINE M_vect_int_op_def

OPERATIONS

yy <-- vect_int_op_def(xx, zz) =
 PRE
   zz : INT &
   xx : INT
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 2 )
 END 
END
