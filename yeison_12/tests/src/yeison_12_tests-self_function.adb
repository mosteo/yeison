with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Self_Function is
   use Operators;
   use all type Kinds;
   use type Reals.General_Real;

begin
   -- Test Self with scalar values
   declare
      -- Test with boolean
      Bool_Value : constant Any := Make.False;
   begin
      -- Change value using Self
      Bool_Value.Self := Make.True;
      Assert (Bool_Value.Kind = Bool_Kind,
              "Value should still be a boolean");
      Assert (Bool_Value.As_Bool = True,
              "Value should be changed to True");
   end;

   declare
      -- Test with integer
      Int_Value : constant Any := +42;
   begin
      -- Change value using Self
      Int_Value.Self := +100;
      Assert (Int_Value.Kind = Int_Kind,
              "Value should still be an integer");
      Assert (Int_Value.As_Int = 100,
              "Value should be changed to 100");
   end;

   declare
      -- Test with real
      Real_Value : constant Any := +3.14159;
   begin
      -- Change value using Self
      Real_Value.Self := +2.71828;
      Assert (Real_Value.Kind = Real_Kind,
              "Value should still be a real");
      Assert (Real_Value.As_Real = Reals.New_Real (2.71828),
              "Value should be changed to 2.71828");
   end;

   declare
      -- Test with string
      Str_Value : constant Any := +"original";
   begin
      -- Change value using Self
      Str_Value.Self := +"modified";
      Assert (Str_Value.Kind = Str_Kind,
              "Value should still be a string");
      Assert (Str_Value.As_Text = "modified",
              "Value should be changed to 'modified'");
   end;

   -- Test Self with composite values
   declare
      -- Test with map
      Map_Value : constant Any := Empty_Map.Insert (+"key", +"value");
   begin
      -- Change value using Self
      Map_Value.Self := Empty_Map.Insert (+"new_key", +"new_value");
      Assert (Map_Value.Kind = Map_Kind,
              "Value should still be a map");
      Assert (Map_Value.Length = 1,
              "Map should have 1 element");
      Assert (Map_Value ("new_key").As_Text = "new_value",
              "Map should have key 'new_key' with value 'new_value'");
      Assert (not Map_Value.Has_Key ("key"),
              "Original key should no longer exist");
   end;

   declare
      -- Test with vector
      Vec_Value : constant Any := Empty_Vec.Append (+1).Append (+2);
   begin
      -- Change value using Self
      Vec_Value.Self := Empty_Vec.Append (+3).Append (+4).Append (+5);
      Assert (Vec_Value.Kind = Vec_Kind,
              "Value should still be a vector");
      Assert (Vec_Value.Length = 3,
              "Vector should have 3 elements");
      Assert (Vec_Value (1).As_Int = 3,
              "First element should be 3");
      Assert (Vec_Value (2).As_Int = 4,
              "Second element should be 4");
      Assert (Vec_Value (3).As_Int = 5,
              "Third element should be 5");
   end;

   -- Test changing type using Self
   declare
      -- Start with integer
      Value : constant Any := +42;
   begin
      -- Change to string
      Value.Self := +"string";
      Assert (Value.Kind = Str_Kind,
              "Value should be changed to string");
      Assert (Value.As_Text = "string",
              "Value should be 'string'");

      -- Change to boolean
      Value.Self := Make.True;
      Assert (Value.Kind = Bool_Kind,
              "Value should be changed to boolean");
      Assert (Value.As_Bool = True,
              "Value should be True");

      -- Change to map
      Value.Self := Empty_Map.Insert (+"key", +"value");
      Assert (Value.Kind = Map_Kind,
              "Value should be changed to map");
      Assert (Value ("key").As_Text = "value",
              "Map should have key 'key' with value 'value'");

      -- Change to vector
      Value.Self := Empty_Vec.Append (+1).Append (+2);
      Assert (Value.Kind = Vec_Kind,
              "Value should be changed to vector");
      Assert (Value.Length = 2,
              "Vector should have 2 elements");
   end;

   -- Test Self with nil value
   declare
      -- Start with nil
      Nil_Value : constant Any := Make.Nil;
   begin
      -- Change to integer
      Nil_Value.Self := +42;
      Assert (Nil_Value.Kind = Int_Kind,
              "Value should be changed to integer");
      Assert (Nil_Value.As_Int = 42,
              "Value should be 42");
   end;

   -- Test Self with nested structures
   declare
      -- Create a nested structure
      Nested : constant Any := Empty_Map
        .Insert (+"level1", Empty_Map
                 .Insert (+"level2", Empty_Vec.Append (+1).Append (+2)));
   begin
      -- Modify a nested value using Self
      Nested ("level1") ("level2").Self := Empty_Vec.Append (+3).Append (+4);

      -- Verify the change
      Assert (Nested ("level1") ("level2").Kind = Vec_Kind,
              "Nested value should still be a vector");
      Assert (Nested ("level1") ("level2").Length = 2,
              "Nested vector should have 2 elements");
      Assert (Nested ("level1") ("level2") (1).As_Int = 3,
              "First element should be 3");
      Assert (Nested ("level1") ("level2") (2).As_Int = 4,
              "Second element should be 4");
   end;

   -- Test Self with empty map and vector
   declare
      -- Start with empty map
      Empty_M : constant Any := Empty_Map;
   begin
      -- Change to non-empty map
      Empty_M.Self := Empty_Map.Insert (+"key", +"value");
      Assert (Empty_M.Kind = Map_Kind,
              "Value should still be a map");
      Assert (Empty_M.Length = 1,
              "Map should have 1 element");

      -- Change back to empty map
      Empty_M.Self := Empty_Map;
      Assert (Empty_M.Kind = Map_Kind,
              "Value should still be a map");
      Assert (Empty_M.Length = 0,
              "Map should be empty");
   end;

   declare
      -- Start with empty vector
      Empty_V : constant Any := Empty_Vec;
   begin
      -- Change to non-empty vector
      Empty_V.Self := Empty_Vec.Append (+1);
      Assert (Empty_V.Kind = Vec_Kind,
              "Value should still be a vector");
      Assert (Empty_V.Length = 1,
              "Vector should have 1 element");

      -- Change back to empty vector
      Empty_V.Self := Empty_Vec;
      Assert (Empty_V.Kind = Vec_Kind,
              "Value should still be a vector");
      Assert (Empty_V.Length = 0,
              "Vector should be empty");
   end;
end Yeison_12_Tests.Self_Function;
