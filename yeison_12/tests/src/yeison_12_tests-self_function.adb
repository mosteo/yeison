with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Self_Function is
   use Operators;
   use type Reals.General_Real;

begin
   --  Test in-place replacement of scalar values

   declare
      Bool_Value : Any := Make.False;
   begin
      Bool_Value := Make.True;
      Assert (Bool_Value.Kind = Bool_Kind,
              "Value should still be a boolean");
      Assert (Bool_Value.As_Bool = True,
              "Value should be changed to True");
   end;

   declare
      Int_Value : Any := +42;
   begin
      Int_Value := +100;
      Assert (Int_Value.Kind = Int_Kind,
              "Value should still be an integer");
      Assert (Int_Value.As_Int = 100,
              "Value should be changed to 100");
   end;

   declare
      Real_Value : Any := +3.14159;
   begin
      Real_Value := +2.71828;
      Assert (Real_Value.Kind = Real_Kind,
              "Value should still be a real");
      Assert (Real_Value.As_Real = Reals.New_Real (2.71828),
              "Value should be changed to 2.71828");
   end;

   declare
      Str_Value : Any := +"original";
   begin
      Str_Value := +"modified";
      Assert (Str_Value.Kind = Str_Kind,
              "Value should still be a string");
      Assert (Str_Value.As_Text = "modified",
              "Value should be changed to 'modified'");
   end;

   --  Test in-place replacement of composite values

   declare
      Map_Value : Any := Empty_Map.Insert (+"key", +"value");
   begin
      Map_Value := Empty_Map.Insert (+"new_key", +"new_value");
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
      Vec_Value : Any := Empty_Vec.Append (+1).Append (+2);
   begin
      Vec_Value := Empty_Vec.Append (+3).Append (+4).Append (+5);
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

   --  Test type change via reassignment

   declare
      Value : Any := +42;
   begin
      Value := +"string";
      Assert (Value.Kind = Str_Kind,
              "Value should be changed to string");
      Assert (Value.As_Text = "string",
              "Value should be 'string'");

      Value := Make.True;
      Assert (Value.Kind = Bool_Kind,
              "Value should be changed to boolean");
      Assert (Value.As_Bool = True,
              "Value should be True");

      Value := Empty_Map.Insert (+"key", +"value");
      Assert (Value.Kind = Map_Kind,
              "Value should be changed to map");
      Assert (Value ("key").As_Text = "value",
              "Map should have key 'key' with value 'value'");

      Value := Empty_Vec.Append (+1).Append (+2);
      Assert (Value.Kind = Vec_Kind,
              "Value should be changed to vector");
      Assert (Value.Length = 2,
              "Vector should have 2 elements");
   end;

   declare
      Nil_Value : Any := Make.Nil;
   begin
      Nil_Value := +42;
      Assert (Nil_Value.Kind = Int_Kind,
              "Value should be changed to integer");
      Assert (Nil_Value.As_Int = 42,
              "Value should be 42");
   end;

   --  Test in-place replacement of a nested element via variable indexing

   declare
      Nested : Any := Empty_Map
        .Insert (+"level1", Empty_Map
                 .Insert (+"level2", Empty_Vec.Append (+1).Append (+2)));
   begin
      Nested ("level1") ("level2") := Empty_Vec.Append (+3).Append (+4);
      Assert (Nested ("level1") ("level2").Kind = Vec_Kind,
              "Nested value should still be a vector");
      Assert (Nested ("level1") ("level2").Length = 2,
              "Nested vector should have 2 elements");
      Assert (Nested ("level1") ("level2") (1).As_Int = 3,
              "First element should be 3");
      Assert (Nested ("level1") ("level2") (2).As_Int = 4,
              "Second element should be 4");
   end;

   declare
      Empty_M : Any := Empty_Map;
   begin
      Empty_M := Empty_Map.Insert (+"key", +"value");
      Assert (Empty_M.Kind = Map_Kind,
              "Value should still be a map");
      Assert (Empty_M.Length = 1,
              "Map should have 1 element");

      Empty_M := Empty_Map;
      Assert (Empty_M.Kind = Map_Kind,
              "Value should still be a map");
      Assert (Empty_M.Length = 0,
              "Map should be empty");
   end;

   declare
      Empty_V : Any := Empty_Vec;
   begin
      Empty_V := Empty_Vec.Append (+1);
      Assert (Empty_V.Kind = Vec_Kind,
              "Value should still be a vector");
      Assert (Empty_V.Length = 1,
              "Vector should have 1 element");

      Empty_V := Empty_Vec;
      Assert (Empty_V.Kind = Vec_Kind,
              "Value should still be a vector");
      Assert (Empty_V.Length = 0,
              "Vector should be empty");
   end;
end Yeison_12_Tests.Self_Function;
