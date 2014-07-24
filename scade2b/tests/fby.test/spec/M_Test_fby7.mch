MACHINE M_Test_fby7

OPERATIONS

yy <-- Test_fby7(xx, P_INI) =
 PRE
   P_INI : seq(INT) & dom(P_INI) = 1 .. 2 &
   xx : seq(INT) & dom(xx) = 1 .. 2
 THEN
   yy : ( yy : seq(INT) & dom(yy) = 1 .. 2 )
 END 
END
