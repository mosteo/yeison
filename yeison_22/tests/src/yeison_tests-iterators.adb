pragma Ada_2022;

procedure Yeison_Tests.Iterators is
   V : constant Any := +[10, 20, 30];
   M : constant Any := ["a" => 1, "b" => 2, "c" => 3];
begin
   --  "for E of Vec" yields the elements
   declare
      Sum : Big_Int := 0;
   begin
      for E of V loop
         Sum := Sum + E.As_Int;
      end loop;
      Assert (Sum = 60, "vector iteration sum");
   end;

   --  "for E of Map" yields the values
   declare
      Sum : Big_Int := 0;
   begin
      for E of M loop
         Sum := Sum + E.As_Int;
      end loop;
      Assert (Sum = 6, "map values iteration sum");
   end;

   --  Cursor-based traversal with Key/Element over a map
   declare
      It    : constant Iteration.Forward_Iterator'Class := Iterate (M);
      Count : Natural := 0;
      C     : Cursor  := It.First;
   begin
      while Has_Element (C) loop
         Count := Count + 1;
         --  Key/Element consistency: M (Key) = Element
         Assert (M (M.Key (C)) = M.Element (C), "key/element consistency");
         C := It.Next (C);
      end loop;
      Assert (Count = 3, "three map entries");
   end;
end Yeison_Tests.Iterators;
