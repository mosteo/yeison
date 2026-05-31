with Yeison_Utils;

procedure Yeison_12_Tests.Iterators is
   Sample : Any renames Yeison_12_Tests.Sample;

   Keys   : constant Vec :=
              Empty_Vec
                .Append ("one")
                .Append ("two")
                .Append ("three")
                .Append ("four")
                .Append ("five");

   Sorted_Keys : constant Vec :=
                   Empty_Vec
                     .Append ("five")
                     .Append ("four")
                     .Append ("one")
                     .Append ("three")
                     .Append ("two");
begin
   Assert (Sample.Keys.Length = 5, "Length:" & Sample.Keys.Length'Image);

   declare
      I : Big_Int := 1;
   begin
      for Key of Sample.Keys loop
         Assert (Key = Keys (I),
                 Yeison_Utils.Encode (Key.Image) & " /=" & I'Image);
         I := I + 1;
      end loop;
   end;

   declare
      I : Big_Int := 1;
   begin
      for Key of Sample.Keys (Ordered => True) loop
         Assert (Key = Sorted_Keys (I),
                 Yeison_Utils.Encode (Key.Image) & " /=" & I'Image);
         I := I + 1;
      end loop;
   end;

   Assert (Keys.Element (Keys.First) = "one");

   --  Key: iterate over a map and retrieve both key and value via cursor
   declare
      M : constant Any :=
            Empty_Map
              .Insert (+"alpha", +1)
              .Insert (+"beta",  +2);
      C : Cursor := M.First;
   begin
      Assert (M.Key (C) = "alpha", "first map key via cursor");
      Assert (M.Element (C).As_Int = 1, "first map value via cursor");
      C := M.Iterate.Next (C);
      Assert (M.Key (C) = "beta", "second map key via cursor");
      Assert (M.Element (C).As_Int = 2, "second map value via cursor");
   end;
end Yeison_12_Tests.Iterators;
