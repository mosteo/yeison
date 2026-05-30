with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Index_Validation is
   use Operators;

   Test_Vec : Any := Empty_Vec;
begin
   -- Add some elements to the vector
   Test_Vec.Append (+1);
   Test_Vec.Append (+2);
   Test_Vec.Append (+3);

   -- Test accessing with a valid index
   declare
      Valid_Access : Any;
   begin
      Valid_Access := Test_Vec (2);
      Assert (Valid_Access.As_Int = 2, "Element at index 2 should be 2");
   end;

   -- Test accessing with a zero index (should raise Constraint_Error)
   declare
      Zero_Index_Access : Any with Unreferenced;
      Success : Boolean := False;
   begin
      begin
         Zero_Index_Access := Test_Vec (0);
         Success := True; -- Should not reach this point
      exception
         when Constraint_Error =>
            null; -- Expected exception
      end;
      Assert (not Success, "Zero index should raise Constraint_Error");
   end;

   -- Test accessing with a negative index (should raise Constraint_Error)
   declare
      Negative_Index_Access : Any with Unreferenced;
      Success : Boolean := False;
   begin
      begin
         Negative_Index_Access := Test_Vec (-1);
         Success := True; -- Should not reach this point
      exception
         when Constraint_Error =>
            null; -- Expected exception
      end;
      Assert (not Success, "Negative index should raise Constraint_Error");
   end;

   -- Test accessing with an index that is more than one beyond the current length
   -- (should raise Constraint_Error)
   declare
      Far_Beyond_Access : Any with Unreferenced;
      Success : Boolean := False;
   begin
      begin
         Far_Beyond_Access := Test_Vec (5); -- Length is 3, so 5 is too far
         Success := True; -- Should not reach this point
      exception
         when Constraint_Error =>
            null; -- Expected exception
      end;
      Assert (not Success,
              "Index beyond length+1 should raise Constraint_Error");
   end;

   -- Test READING one beyond the current length: constant indexing requires a
   -- valid index, so this must raise (it must not grow the vector).
   declare
      Just_Beyond_Access : Any with Unreferenced;
      Success : Boolean := False;
   begin
      begin
         Just_Beyond_Access := Test_Vec (4); -- Length is 3, so 4 is invalid
         Success := True; -- Should not reach this point
      exception
         when Constraint_Error =>
            null; -- Expected exception
      end;
      Assert (not Success,
              "Reading beyond length should raise Constraint_Error");
      Assert (Test_Vec.Length = 3,
              "A failed read must not grow the vector");
   end;

   -- Test WRITING at exactly one beyond the current length: variable indexing
   -- creates the new (initially Nil) position, then receives the value.
   begin
      Test_Vec (4) := +"grown"; -- Length is 3, so 4 is just beyond
      Assert (Test_Vec.Length = 4,
              "Vector should grow when writing at length+1");
      Assert (Test_Vec (4).As_Text = "grown",
              "Newly created element should hold the assigned value");
   end;
end Yeison_12_Tests.Index_Validation;
