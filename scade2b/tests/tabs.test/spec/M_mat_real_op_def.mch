MACHINE M_mat_real_op_def

OPERATIONS

yy <-- mat_real_op_def(xx, zz) =
 PRE
   zz : seq(REAL) & dom(zz) = 1 .. 2 &
   xx : seq(REAL) & dom(xx) = 1 .. 2 & !jj_index.(jj_index : dom(xx) => ((x(jj_index) > 0.000000) & (x(jj_index) < 1000.000000)))
 THEN
   yy : ( yy : seq(seq(REAL)) & dom(yy) = 1 .. 2  & !jj_index.(jj_index : dom(yy) => ((conc(y)(jj_index) > 0.000000) & (conc(y)(jj_index) < 1000.000000))) )
 END 
END
