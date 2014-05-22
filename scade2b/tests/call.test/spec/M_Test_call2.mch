MACHINE M_Test_call2
SEES M_Enum
OPERATIONS

rr <-- Test_call2(xx) =
 PRE
   xx : INT
 THEN
   rr :: { ii_ | ii_ : INT }
 END 
END
