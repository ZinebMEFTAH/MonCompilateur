VAR
  a, b, c, i, j, counter, result: INTEGER;
  d1, d2, d3, dTotal: DOUBLE;
  flag, cond, success: BOOLEAN;
  letter: CHAR.

BEGIN
  a := 3;
  b := 2;
  c := 1;

  d1 := 2.0;
  d2 := 1.5;


  d3 := (a + 1) * ((d1 + a) * ((a - c) + b));  // → 2.0 + 1.5 * 4 = 2.0 + 6.0 = 8.0
  DISPLAY d3;



END.
