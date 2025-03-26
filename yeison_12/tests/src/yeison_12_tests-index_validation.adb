with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Index_Validation is
   use Operators;
   use all type Yeison_12.Kinds;

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

   -- Test accessing with an index that is exactly one beyond the current length
   -- (should append a new element)
   declare
      Just_Beyond_Access : Any;
   begin
      Just_Beyond_Access := Test_Vec (4); -- Length is 3, so 4 is just beyond
      Assert (Test_Vec.Length = 4,
              "Vector should grow when accessing at length+1");
      Assert (Just_Beyond_Access.Kind = Nil_Kind,
              "New element should be Nil");
   end;
end Yeison_12_Tests.Index_Validation;
