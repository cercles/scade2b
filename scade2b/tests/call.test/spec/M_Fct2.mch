MACHINE M_Fct2
SEES M_Enum
OPERATIONS

r1, r2 <-- Fct2(xx) =
 PRE
   xx : INT
 THEN
   r2 :: { ii | ii : INT }||
   r1 :: { ii | ii : INT }
 END 
END
