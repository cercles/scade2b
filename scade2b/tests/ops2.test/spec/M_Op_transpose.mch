MACHINE M_Op_transpose

OPERATIONS

rr <-- Op_transpose(xx) =
 PRE
   xx : seq(seq(INT)) & dom(xx) = 1 .. 2 
 THEN
   rr : ( rr : seq(seq(INT)) & dom(rr) = 1 .. 3  )
 END 
END
