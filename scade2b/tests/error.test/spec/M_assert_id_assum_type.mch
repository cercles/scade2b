MACHINE M_assert_id_assum_type
OPERATIONS

rr <-- assert_id_assum_type(bb) =
 PRE
   bb : INT
 THEN
   rr : ( ii | ii : INT )
 END
END
