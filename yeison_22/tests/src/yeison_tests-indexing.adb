pragma Ada_2022;

procedure Yeison_Tests.Indexing is
   M : constant Any := ["a" => ["b" => 42]];
   V : constant Any := +[+[1, 2], +[3, 4]];
begin
   --  Nested map indexing, chained and path forms
   Assert (M ("a") ("b").As_Int = 42,      "chained map indexing");
   Assert (M ("a" / "b").As_Int = 42,      "path map indexing");
   Assert (M (+["a", "b"]).As_Int = 42,    "vector-path map indexing");

   --  Nested vector indexing
   Assert (V (1) (2).As_Int = 2,           "chained vec indexing");
   Assert (V (2) (1).As_Int = 3,           "chained vec indexing 2");
   Assert (V.Get (2 / 2).As_Int = 4,       "path vec indexing");

   --  Mutation through variable indexing (zero-copy, native)
   declare
      Map : Any;
   begin
      Map ("x") := "y";
      Assert (Map.Kind = Map_Kind,         "auto-vivified map");
      Assert (Map ("x").As_Text = "y",     "map mutation");
      Map ("x") := 1;                       --  overwrite, changing kind
      Assert (Map ("x").As_Int = 1,        "map overwrite");
   end;

   --  Deep nested mutation
   declare
      Deep : Any;
   begin
      Deep ("a") ("b") := 99;
      Assert (Deep ("a") ("b").As_Int = 99, "deep nested mutation");
   end;

   --  Constant indexing returns a copy that does not alter the original
   declare
      Orig : constant Any := ["k" => 1];
      Copy : constant Any := Orig ("k");
   begin
      Assert (Copy.As_Int = 1,             "constant indexing copy");
   end;
end Yeison_Tests.Indexing;
