MACHINE M_mat_real_op_def

OPERATIONS

yy <-- mat_real_op_def(xx, zz) =
 PRE
   zz : seq(REAL) &
   xx : seq(REAL) & !jj.(jj : INT => ((xx(jj) > 0.000000) & (xx(jj) < 1000.000000)))
 THEN
   yy :: { ii | ii : seq(seq(REAL)) & !jj.(jj : INT => ((conc(ii)(jj) > 0.000000) & (conc(ii)(jj) < 1000.000000))) } 
 END 
END
