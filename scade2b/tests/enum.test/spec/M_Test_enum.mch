MACHINE M_Test_enum
SEES M_Enum
OPERATIONS

rr <-- Test_enum(xx) =
 PRE
   xx : Day
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
