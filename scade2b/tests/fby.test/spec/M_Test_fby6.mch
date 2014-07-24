MACHINE M_Test_fby6

OPERATIONS

yy <-- Test_fby6(xx, P_INI) =
 PRE
   P_INI : seq(INT) & dom(P_INI) = 1 .. 2 &
   xx : seq(INT) & dom(xx) = 1 .. 2 & !jj.(jj : dom(xx) => (xx(jj) > 0))
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 2 )
 END 
END
