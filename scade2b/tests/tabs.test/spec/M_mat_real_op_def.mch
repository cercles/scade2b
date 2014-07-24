MACHINE M_mat_real_op_def

OPERATIONS

yy <-- mat_real_op_def(xx, zz) =
 PRE
   zz : seq(REAL) & dom(zz) = 1 .. 2 &
   xx : seq(REAL) & dom(xx) = 1 .. 2 & !jj.(jj : dom(xx) => ((xx(jj) > 0.000000) & (xx(jj) < 1000.000000)))
 THEN
   yy : ( yy : seq(seq(REAL)) & dom(yy) = 1 .. 2  & !jj.(jj : dom(yy) => ((conc(y)(jj) > 0.000000) & (conc(y)(jj) < 1000.000000))) )
 END 
END
