pragma Ada_2022;

procedure Yeison_Tests.Vectors is
   Homo   : constant Any := +[1, 2, 3];
   Hetero : constant Any := +[1, "two", 3.0];
begin
   Assert (Homo.Kind = Vec_Kind,    "vector kind");
   Assert (Homo.Length = 3,         "vector length");
   Assert (Homo.First_Index = 1,    "1-based first index");
   Assert (Homo.Last_Index = 3,     "last index");
   Assert (Homo (1).As_Int = 1,     "first element");
   Assert (Homo (3).As_Int = 3,     "third element");

   Assert (Hetero (2).As_Text = "two", "heterogeneous element");
   Assert (Hetero (3).As_Real_Float = 3.0, "real element");

   --  Head/Tail
   Assert (Homo.Head.As_Int = 1,    "head");
   Assert (Homo.Tail.Length = 2,    "tail length");
   declare
      T : constant Any := Homo.Tail;
   begin
      Assert (T (1).As_Int = 2,     "tail first");
   end;

   --  Append (functional and procedural)
   declare
      V : Any := +[1];
   begin
      V.Append (2);
      Assert (V.Length = 2,         "appended length");
      Assert (V (2).As_Int = 2,     "appended value");

      declare
         V3 : constant Any := V.Append (3);
      begin
         Assert (V3.Length = 3,     "functional append copy");
         Assert (V.Length = 2,      "original unchanged");
      end;
   end;

   --  Grow one element at a time via indexing (1 past the end)
   declare
      V : Any;
   begin
      V (1) := "one";
      V (2) := "two";
      Assert (V.Kind = Vec_Kind,    "auto-vivified vector");
      Assert (V.Length = 2,         "grown to 2");
      Assert (V (1).As_Text = "one", "grown element 1");
   end;

   --  Prepend (procedural and functional)
   declare
      V : Any := +[2, 3];
   begin
      V.Prepend (1);
      Assert (V.Length = 3,          "prepended length");
      Assert (V (1).As_Int = 1,      "prepended at front");
      Assert (V (2).As_Int = 2,      "old front shifted");

      declare
         V0 : constant Any := V.Prepend (0);
      begin
         Assert (V0.Length = 4,      "functional prepend copy");
         Assert (V0 (1).As_Int = 0,  "functional prepend front");
         Assert (V.Length = 3,       "original unchanged by prepend");
      end;
   end;

   --  Contains
   declare
      V : constant Any := +[1, "two", 3.0];
   begin
      Assert (V.Contains (1),        "contains int element");
      Assert (V.Contains ("two"),    "contains str element");
      Assert (V.Contains (3.0),      "contains real element");
      Assert (not V.Contains (99),   "does not contain absent");
   end;
end Yeison_Tests.Vectors;
