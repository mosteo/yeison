with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Reference_Keys is
   use Operators;

   Test_Map : Any := Empty_Map;
begin
   -- Create a map with a few entries
   Test_Map.Insert (+"key1", +"value1");
   Test_Map.Insert (+"key2", +"value2");

   -- Check initial state
   Assert (Test_Map.Length = 2, "Map should have 2 elements");
   Assert (Test_Map.Keys.Length = 2, "Keys vector should have 2 elements");

   -- Access a non-existent key through Reference, which should create it
   Test_Map.Reference (+"key3").Element.all := +"value3";

   -- Check that both the map and keys vector were updated
   Assert (Test_Map.Length = 3, "Map should have 3 elements after Reference");
   Assert (Test_Map.Keys.Length = 3,
           "Keys vector should have 3 elements after Reference");
end Yeison_12_Tests.Reference_Keys;
