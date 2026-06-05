pragma Ada_2022;

procedure Yeison_Tests.Maps is
   Empty : constant Any := [];
   M     : constant Any := ["one" => 1, "two" => "two"];
begin
   Assert (Empty.Kind = Map_Kind, "[] is an empty map");
   Assert (Empty.Is_Empty,        "empty map is empty");

   Assert (M.Kind = Map_Kind,     "map aggregate kind");
   Assert (M.Length = 2,          "map length");
   Assert (M.Has_Key ("one"),     "has key one");
   Assert (not M.Has_Key ("nope"), "missing key");

   Assert (M ("one").As_Int = 1,    "value of one");
   Assert (M ("two").As_Text = "two", "value of two");

   --  Keys, in insertion order
   declare
      K : constant Any := M.Keys;
   begin
      Assert (K.Kind = Vec_Kind,    "keys is a vector");
      Assert (K.Length = 2,         "two keys");
      Assert (K (1).As_Text = "one", "first key");
      Assert (K (2).As_Text = "two", "second key");
   end;

   --  Functional insert returns a copy, original unchanged
   declare
      M2 : constant Any := M.Insert ("three", 3);
   begin
      Assert (M2.Length = 3,        "inserted copy has 3");
      Assert (M.Length = 2,         "original still 2");
   end;

   --  In-place procedural insert
   declare
      V : Any := ["a" => 1];
   begin
      V.Insert ("b", 2);
      Assert (V.Length = 2,         "in-place insert");
      Assert (V ("b").As_Int = 2,   "in-place value");
   end;

   --  Contains tests value membership (not keys; use Has_Key for keys)
   declare
      M : constant Any := ["one" => 1, "two" => "dos"];
   begin
      Assert (M.Contains (1),         "map contains value 1");
      Assert (M.Contains ("dos"),     "map contains value dos");
      Assert (not M.Contains ("one"), "a key is not a value");
      Assert (not M.Contains (99),    "absent value");
   end;
end Yeison_Tests.Maps;
