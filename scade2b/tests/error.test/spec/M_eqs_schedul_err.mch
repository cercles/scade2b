MACHINE M_eqs_schedul_err
OPERATIONS

rr <-- eqs_schedul_err(bb) =
 PRE
   bb : INT
 THEN
   rr :: { ii | ii : INT }
 END
END
