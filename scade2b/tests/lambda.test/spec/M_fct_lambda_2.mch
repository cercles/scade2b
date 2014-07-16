MACHINE M_fct_lambda_2(aa)

CONSTRAINTS
  aa : INT
OPERATIONS

rr <-- fct_lambda_2 =
 PRE
   TRUE = TRUE
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
