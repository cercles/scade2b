node etat_init
  (A_0: bool;
  A_1: bool;
  B_0: bool;
  B_1: bool)
returns
  (etat_0: bool;
  etat_1: bool);

let
  etat_0 = (if A_0 then B_0 else true);
  etat_1 = (if A_1 then B_1 else true);
tel

