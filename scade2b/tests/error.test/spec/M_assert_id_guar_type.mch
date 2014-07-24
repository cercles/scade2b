MACHINE M_assert_id_guar_type
OPERATIONS

rr <-- assert_id_guar_type(bb) =
 PRE
   bb : INT
 THEN
   rr : ( ii | ii : INT )
 END
END
