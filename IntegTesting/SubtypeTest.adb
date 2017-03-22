with Ada.Text_IO; use Ada.Text_IO;

procedure SubtypeTest is

  type sub is digits 4 range 0.0 .. 500.0;

  type subArray is array (1..4) of sub;

  arr : subArray;
  avg : sub := 0.0;

begin

  for i in arr'Range loop
    arr(i) := 500.0;
  end loop;

  for n in arr'Range loop
    avg := avg + (arr(n) - avg)/sub(n);
  end loop;

  Put_Line("Avg = " & avg'Img);

end SubtypeTest;
