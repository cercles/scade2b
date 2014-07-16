MACHINE M_register_error_type

OPERATIONS

rr <-- register_error_type(aa, bb) =
 PRE
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
