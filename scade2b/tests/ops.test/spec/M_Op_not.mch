MACHINE M_Op_not
SEES M_Enum
OPERATIONS

rr <-- Op_not(aa) =
 PRE
   aa : BOOL
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
