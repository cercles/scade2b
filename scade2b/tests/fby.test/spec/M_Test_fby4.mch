MACHINE M_Test_fby4

OPERATIONS

yy, zz <-- Test_fby4(xx) =
 PRE
   xx : INT
 THEN
   zz : ( zz : INT )||
   yy : ( yy : INT & (yy > 0) )
 END 
END
