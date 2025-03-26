with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Nested_Indexing is
   use Operators;
   use all type Kinds;

   -- Create a deeply nested structure for testing
   Nested_Map : constant Any := Empty_Map
     .Insert (+"level1", Empty_Map
              .Insert (+"level2", Empty_Map
                       .Insert (+"level3", Empty_Map
                                .Insert (+"level4", Empty_Map
                                         .Insert (+"level5", +"deep value")))))
     .Insert (+"array_path", Empty_Map
              .Insert (+"nested_array", To_Vec ((
                +1,
                +2,
                To_Vec ((
                  +"a",
                  +"b",
                  +"c"
                )),
                Empty_Map.Insert (+"key", +"value")
              ))))
     .Insert (+"mixed_path", To_Vec ((
                Empty_Map.Insert (+"map_in_array", +"found me"),
                To_Vec ((
                  1 => Empty_Map.Insert (+"deeper", +"nested value")
                ))
              )));

begin
   -- Test basic nested map indexing (5 levels deep)
   declare
      Deep_Value : constant Any :=
        Nested_Map ("level1") ("level2") ("level3") ("level4") ("level5");
   begin
      Assert (Deep_Value.Kind = Str_Kind,
              "Deep value should be a string");
      Assert (Deep_Value.As_Text = "deep value",
              "Deep value should be 'deep value'");
   end;

   -- Test nested indexing with arrays
   declare
      Array_Value : constant Any := Nested_Map ("array_path") ("nested_array");
   begin
      Assert (Array_Value.Kind = Vec_Kind,
              "Array value should be a vector");
      Assert (Array_Value.Length = 4,
              "Array should have 4 elements");

      -- Test indexing into the array
      Assert (Array_Value (1).As_Int = 1,
              "First array element should be 1");
      Assert (Array_Value (2).As_Int = 2,
              "Second array element should be 2");

      -- Test nested array within the array
      declare
         Nested_Array : constant Any := Array_Value (3);
      begin
         Assert (Nested_Array.Kind = Vec_Kind,
                 "Third element should be a vector");
         Assert (Nested_Array.Length = 3,
                 "Nested array should have 3 elements");
         Assert (Nested_Array (1).As_Text = "a",
                 "First nested array element should be 'a'");
         Assert (Nested_Array (2).As_Text = "b",
                 "Second nested array element should be 'b'");
         Assert (Nested_Array (3).As_Text = "c",
                 "Third nested array element should be 'c'");
      end;

      -- Test map within the array
      declare
         Map_In_Array : constant Any := Array_Value (4);
      begin
         Assert (Map_In_Array.Kind = Map_Kind,
                 "Fourth element should be a map");
         Assert (Map_In_Array ("key").As_Text = "value",
                 "Map in array should have key 'key' with value 'value'");
      end;
   end;

   -- Test mixed path with both maps and arrays
   declare
      Mixed_Path : constant Any := Nested_Map ("mixed_path");
   begin
      Assert (Mixed_Path.Kind = Vec_Kind,
              "Mixed path should be a vector");

      -- Test map in array
      Assert (Mixed_Path (1).Kind = Map_Kind,
              "First element should be a map");
      Assert (Mixed_Path (1) ("map_in_array").As_Text = "found me",
              "Map in array should have key 'map_in_array' with value 'found me'");

      -- Test nested array with map
      Assert (Mixed_Path (2).Kind = Vec_Kind,
              "Second element should be a vector");
      Assert (Mixed_Path (2) (1).Kind = Map_Kind,
              "First element of nested array should be a map");
      Assert (Mixed_Path (2) (1) ("deeper").As_Text = "nested value",
              "Nested map should have key 'deeper' with value 'nested value'");
   end;

   -- Test chained indexing in a single expression
   Assert (Nested_Map ("level1") ("level2") ("level3") ("level4") ("level5").As_Text = "deep value",
           "Chained indexing should work in a single expression");

   -- Test even more complex chained indexing
   Assert (Nested_Map ("array_path") ("nested_array") (3) (2).As_Text = "b",
           "Complex chained indexing with arrays should work");

   -- Test indexing with string literals directly
   Assert (Nested_Map ("level1") ("level2") ("level3") ("level4") ("level5") = "deep value",
           "Indexing with string literals and comparison should work");

   -- Test creating a new nested structure through indexing
   declare
      Dynamic_Nested : constant Any := Empty_Map;
   begin
      -- Create a deeply nested structure dynamically
      Dynamic_Nested ("level1") ("level2") ("level3") ("level4") ("level5") := +"dynamic value";

      -- Verify the structure was created correctly
      Assert (Dynamic_Nested.Kind = Map_Kind,
              "Root should be a map");
      Assert (Dynamic_Nested ("level1").Kind = Map_Kind,
              "Level 1 should be a map");
      Assert (Dynamic_Nested ("level1") ("level2").Kind = Map_Kind,
              "Level 2 should be a map");
      Assert (Dynamic_Nested ("level1") ("level2") ("level3").Kind = Map_Kind,
              "Level 3 should be a map");
      Assert (Dynamic_Nested ("level1") ("level2") ("level3") ("level4").Kind = Map_Kind,
              "Level 4 should be a map");
      Assert (Dynamic_Nested ("level1") ("level2") ("level3") ("level4") ("level5").Kind = Str_Kind,
              "Level 5 should be a string");
      Assert (Dynamic_Nested ("level1") ("level2") ("level3") ("level4") ("level5").As_Text = "dynamic value",
              "Level 5 value should be 'dynamic value'");
   end;

   -- Test creating a nested structure with mixed maps and arrays
   declare
      Mixed_Dynamic : constant Any := Empty_Map;
   begin
      -- Create a structure with both maps and arrays
      Mixed_Dynamic ("array") (1) ("map") (2) := +"mixed value";

      -- Verify the structure was created correctly
      Assert (Mixed_Dynamic.Kind = Map_Kind,
              "Root should be a map");
      Assert (Mixed_Dynamic ("array").Kind = Vec_Kind,
              "First level should be a vector");
      Assert (Mixed_Dynamic ("array") (1).Kind = Map_Kind,
              "Second level should be a map");
      Assert (Mixed_Dynamic ("array") (1) ("map").Kind = Vec_Kind,
              "Third level should be a vector");
      Assert (Mixed_Dynamic ("array") (1) ("map") (2).Kind = Str_Kind,
              "Fourth level should be a string");
      Assert (Mixed_Dynamic ("array") (1) ("map") (2).As_Text = "mixed value",
              "Fourth level value should be 'mixed value'");
   end;
end Yeison_12_Tests.Nested_Indexing;
