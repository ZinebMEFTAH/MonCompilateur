VAR 
    a,b,c   : BOOLEAN ;
    d,e,f,g : INTEGER.

begin
  x := 1;
  y := 2;

  if x == 1 then
    y := 3
  else
    y := 4;

  while x == 1 do
    begin
      y := y + 1;
      x := x - 1
    end;

  for i := 0 to 3 do
    y := y + 1;

  begin
    x := 5;
    y := 6
  end
end.
