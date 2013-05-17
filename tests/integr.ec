node integr
  (x: int)
returns
  (y: int);

var
  V9_r: int;
  V10_z: int;

let
  assert ((-256 <= x) and (x <= 255));
  assert ((-1024 <= y) and (y <= 1023));
  y = (if (V10_z < -1024) then -1024 else (if (V10_z > 1023) then 1023 else 
  V10_z));
  V9_r = (0 -> (pre y));
  V10_z = (x + V9_r);
tel

