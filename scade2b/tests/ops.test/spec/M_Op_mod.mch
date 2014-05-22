MACHINE M_Op_mod
SEES M_Enum
OPERATIONS

rr <-- Op_mod(aa, bb) =
 PRE
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
