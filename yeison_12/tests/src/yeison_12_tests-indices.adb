with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Indices is
   Sample : Any renames Yeison_12_Tests.Sample;

   Test_Vec : constant Vec :=
     Empty_Vec.Append ("one").Append ("two").Append ("three");

   Empty_Vector : constant Vec := Empty_Vec;
begin
   --  Test First_Index and Last_Index on a non-empty vector
   Assert (Test_Vec.First_Index = 1, "First_Index should be 1");
   Assert (Test_Vec.Last_Index = 3, "Last_Index should be 3");

   --  Test First_Index and Last_Index on an empty vector
   --  (should return whatever the underlying container reports)
   Assert
     (Empty_Vector.First_Index = 1, "Empty vector First_Index should be 1");
   Assert (Empty_Vector.Last_Index = 0, "Empty vector Last_Index should be 0");

   --  Test Element with First_Index
   Assert
     (Test_Vec.Element (Test_Vec.First) = "one",
      "First element should be 'one'");

   --  Test Element with Last_Index
   Assert
     (Test_Vec (Test_Vec.Last_Index).As_Text = "three",
      "Last element should be 'three'");

   --  Test iterating using First_Index and Last_Index
   declare
      type Text_Access is access Text;
      type Text_Array is array (1 .. 3) of Text_Access;

      Expected_Values : constant Text_Array :=
        (new Text'("one"), new Text'("two"), new Text'("three"));
      I               : Big_Int := 1;
   begin
      for Idx in Test_Vec.First_Index .. Test_Vec.Last_Index loop
         Assert
           (Test_Vec (Idx).As_Text = Expected_Values (Integer (Idx)).all,
            "Element at index" & Idx'Image);
         I := I + 1;
      end loop;

      Assert (I = 4, "Loop should have executed 3 times");
   end;

   --  Test that Sample vector elements can be accessed with First_Index
   declare
      Vec_Value : constant Any := Sample ("three");
   begin
      Assert (Vec_Value.Kind = Vec_Kind, "Sample('three') should be a vector");
      Assert (Vec_Value.First_Index = 1, "Vector First_Index should be 1");
      Assert (Vec_Value.Last_Index = 3, "Vector Last_Index should be 3");
      Assert
        (Vec_Value (Vec_Value.First_Index).As_Int = 1,
         "First element should be 1");
   end;
end Yeison_12_Tests.Indices;
