MACHINE M_register_error_struct
OPERATIONS

rr <-- register_error_struct(aa, bb) =
 PRE
   aa : INT &
   bb : INT
 THEN
   rr :: { ii | ii : INT }
 END
END
