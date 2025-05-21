VAR
  x, y, i: INTEGER;
  ch: CHAR;
  flag: BOOLEAN;
  d: DOUBLE.

BEGIN
  x := 10;
  y := 5;
  d := 2.5;

  DISPLAY x;
  DISPLAY d;

  IF x > y THEN
    DISPLAY x
  ELSE
    DISPLAY y;

  WHILE x > 0 DO
    BEGIN
      DISPLAY x;
      x := x - 1;
    END;

  DISPLAY d;

  REPEAT
    y := y - 1;
    DISPLAY y;
  UNTIL y == 0;

  DISPLAY d;


  FOR i := 0 TO 5 DO
    DISPLAY i;

  DISPLAY d;
  DISPLAY x;

  CASE x OF
    1: DISPLAY 1;
    2, 0: DISPLAY 2;
    10: DISPLAY 10;
  END;

  DISPLAY d;
  
END.