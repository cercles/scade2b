MACHINE M_Test_fby
SEES M_Enum
OPERATIONS

yy <-- Test_fby(xx) =
 PRE
   xx : INT
 THEN
   yy :: { ii | ii : INT }
 END 
END
