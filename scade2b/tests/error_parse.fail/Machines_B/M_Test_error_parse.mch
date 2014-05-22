MACHINE M_Test_error_parse
OPERATIONS

rr <-- Test_error_parse(xx) =
 PRE
   xx : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
