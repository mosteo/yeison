with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Keys_Consistency is
   use Operators;

   Test_Map : Any := Empty_Map;
begin
   -- Insert a few keys
   Test_Map.Insert (+"key1", +"value1");
   Test_Map.Insert (+"key2", +"value2");
   Test_Map.Insert (+"key3", +"value3");

   -- Check initial state
   Assert (Test_Map.Length = 3, "Map should have 3 elements");
   Assert (Test_Map.Keys.Length = 3, "Keys vector should have 3 elements");

   -- Replace an existing key's value
   Test_Map.Insert (+"key2", +"new_value", Replace => True);

   -- Check that the map and keys vector still have the same length
   Assert (Test_Map.Length = 3,
           "Map should still have 3 elements after Replace");
   Assert (Test_Map.Keys.Length = 3,
           "Keys vector should still have 3 elements after Replace");

   -- Check that the value was actually replaced
   Assert (Test_Map ("key2") = "new_value",
           "Value should be replaced");

   -- Check that all keys are still present in the keys vector
   declare
      Keys : constant Any := Test_Map.Keys;
      Expected_Keys : constant array (1 .. 3) of String (1 .. 4) :=
        ("key1", "key2", "key3");
      Found : array (Expected_Keys'Range) of Boolean := (others => False);
   begin
      for I in Keys.First_Index .. Keys.Last_Index loop
         for J in Expected_Keys'Range loop
            if Keys (I).As_Latin_1 = Expected_Keys (J) then
               Found (J) := True;
            end if;
         end loop;
      end loop;

      for J in Expected_Keys'Range loop
         Assert (Found (J),
                 Expected_Keys (J) & " should be in the keys vector");
      end loop;
   end;
end Yeison_12_Tests.Keys_Consistency;
