MACHINE M_assert_id_assum_2_ids
OPERATIONS

rr <-- assert_id_assum_2_ids(bb) =
 PRE
   bb : INT
 THEN
   rr :: { ii | ii : INT }
 END
END
