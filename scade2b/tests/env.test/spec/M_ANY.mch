MACHINE M_ANY

OPERATIONS

bb <-- ANY(aa) =
 PRE
   aa : INT
 THEN
   bb :: { ii | ii : INT }
 END 
END
