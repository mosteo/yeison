with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Vector_Operations is
   use Operators;
   use type Reals.General_Real;
   use all type Kinds;

   -- Create test vectors
   Empty_V : constant Any := Empty_Vec;
   Single_Element_Vec : constant Any := Empty_Vec.Append (+1);
   Multi_Element_Vec : constant Any :=
     Empty_Vec.Append (+1).Append (+2).Append (+3);
   Mixed_Type_Vec : constant Any :=
     Empty_Vec
       .Append (+1)
       .Append (+"string")
       .Append (Make.True)
       .Append (+3.14159);

   -- Create a reference to test with
   package Refs is new Yeison_12.Impl.References (Any, To_Any);

begin
   -- Test Head function with a single element vector
   declare
      Head_Value : constant Any := Refs.Head (Single_Element_Vec);
   begin
      Assert (Head_Value.As_Int = 1,
              "Head of single element vector should be 1");
   end;

   -- Test Head function with a multi-element vector
   declare
      Head_Value : constant Any := Refs.Head (Multi_Element_Vec);
   begin
      Assert (Head_Value.As_Int = 1,
              "Head of multi-element vector should be 1");
   end;

   -- Test Head function with a mixed-type vector
   declare
      Head_Value : constant Any := Refs.Head (Mixed_Type_Vec);
   begin
      Assert (Head_Value.As_Int = 1,
              "Head of mixed-type vector should be 1");
   end;

   -- Test Tail function with a single element vector
   declare
      Tail_Value : constant Any := Refs.Tail (Single_Element_Vec);
   begin
      Assert (Tail_Value.Length = 0,
              "Tail of single element vector should be empty");
   end;

   -- Test Tail function with a multi-element vector
   declare
      Tail_Value : constant Any := Refs.Tail (Multi_Element_Vec);
   begin
      Assert (Tail_Value.Length = 2,
              "Tail of 3-element vector should have length 2");
      Assert (Tail_Value (1).As_Int = 2,
              "First element of tail should be 2");
      Assert (Tail_Value (2).As_Int = 3,
              "Second element of tail should be 3");
   end;

   -- Test Tail function with a mixed-type vector
   declare
      Tail_Value : constant Any := Refs.Tail (Mixed_Type_Vec);
   begin
      Assert (Tail_Value.Length = 3,
              "Tail of 4-element vector should have length 3");
      Assert (Tail_Value (1).As_Text = "string",
              "First element of tail should be 'string'");
      Assert (Tail_Value (2).As_Bool = True,
              "Second element of tail should be True");
      Assert (Tail_Value (3).As_Real = Reals.New_Real (3.14159),
              "Third element of tail should be 3.14159");
   end;

   -- Test error handling for Head on empty vector
   declare
      Success : Boolean := True;
   begin
      begin
         declare
            Head_Value : constant Any := Refs.Head (Empty_V) with Unreferenced;
         begin
            null;
         end;
         Success := False; -- Should not reach this point
      exception
         when others =>
            null; -- Expected exception
      end;
      Assert (Success, "Head on empty vector should raise an exception");
   end;

   -- Test error handling for Tail on empty vector
   declare
      Success : Boolean := True;
   begin
      begin
         declare
            Tail_Value : constant Any := Refs.Tail (Empty_V) with Unreferenced;
         begin
            null;
         end;
         Success := False; -- Should not reach this point
      exception
         when others =>
            null; -- Expected exception
      end;
      Assert (Success, "Tail on empty vector should raise an exception");
   end;

   -- Test Append with different types
   declare
      Vec : Any := Empty_Vec;
   begin
      -- Append integer
      Vec.Append (+42);
      Assert (Vec.Length = 1, "Vector length should be 1 after appending integer");
      Assert (Vec (1).As_Int = 42, "Appended integer should be 42");

      -- Append string
      Vec.Append (+"string");
      Assert (Vec.Length = 2, "Vector length should be 2 after appending string");
      Assert (Vec (2).As_Text = "string", "Appended string should be 'string'");

      -- Append boolean
      Vec.Append (Make.True);
      Assert (Vec.Length = 3, "Vector length should be 3 after appending boolean");
      Assert (Vec (3).As_Bool = True, "Appended boolean should be True");

      -- Append real
      Vec.Append (+3.14159);
      Assert (Vec.Length = 4, "Vector length should be 4 after appending real");
      Assert (Vec (4).As_Real = Reals.New_Real (3.14159),
              "Appended real should be 3.14159");

      -- Append map
      Vec.Append (Empty_Map.Insert (+"key", +"value"));
      Assert (Vec.Length = 5, "Vector length should be 5 after appending map");
      Assert (Vec (5).Kind = Map_Kind, "Appended element should be a map");
      Assert (Vec (5) ("key").As_Text = "value",
              "Appended map should have key 'key' with value 'value'");

      -- Append vector
      Vec.Append (Empty_Vec.Append (+1).Append (+2));
      Assert (Vec.Length = 6, "Vector length should be 6 after appending vector");
      Assert (Vec (6).Kind = Vec_Kind, "Appended element should be a vector");
      Assert (Vec (6).Length = 2, "Appended vector should have length 2");
   end;

   -- Test functional Append (returns a new vector)
   declare
      Original : constant Any := Empty_Vec.Append (+1);
      New_Vec : constant Any := Original.Append (+2);
   begin
      Assert (Original.Length = 1, "Original vector should still have length 1");
      Assert (Original (1).As_Int = 1, "Original vector should still have element 1");

      Assert (New_Vec.Length = 2, "New vector should have length 2");
      Assert (New_Vec (1).As_Int = 1, "New vector first element should be 1");
      Assert (New_Vec (2).As_Int = 2, "New vector second element should be 2");
   end;
end Yeison_12_Tests.Vector_Operations;
