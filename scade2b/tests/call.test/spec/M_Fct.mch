MACHINE M_Fct
SEES M_Enum
OPERATIONS

rr <-- Fct(xx) =
 PRE
   xx : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
