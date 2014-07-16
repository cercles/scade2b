MACHINE M_collision1

OPERATIONS

ANY_ <-- collision1(aa) =
 PRE
   aa : INT
 THEN
   ANY_ :: { ii | ii : INT }
 END 
END
