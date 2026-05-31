with Yeison_12; use Yeison_12;
with Yeison_Utils;

procedure Yeison_12_Tests.Keys is
   use Operators;

   Test_Map : Any := Empty_Map;
begin
   -- Test with an empty map
   Assert (not Test_Map.Has_Key ("nonexistent"),
           "Has_Key should return False for empty map");

   -- Add some elements to the map
   Test_Map.Insert (+"key1", +"value1");
   Test_Map.Insert (+"key2", +"value2");
   Test_Map.Insert (+"key3", +"value3");

   -- Test with existing keys
   Assert (Test_Map.Has_Key ("key1"),
           "Has_Key should return True for existing key 'key1'");
   Assert (Test_Map.Has_Key ("key2"),
           "Has_Key should return True for existing key 'key2'");
   Assert (Test_Map.Has_Key ("key3"),
           "Has_Key should return True for existing key 'key3'");

   -- Test with non-existent key
   Assert (not Test_Map.Has_Key ("nonexistent"),
           "Has_Key should return False for non-existent key");

   -- Test with empty string key
   Assert (not Test_Map.Has_Key (""),
           "Has_Key should return False for empty string key");

   -- Add an empty string key and test again
   Test_Map.Insert (+"", +"empty key value");
   Assert (Test_Map.Has_Key (""),
           "Has_Key should return True for empty string key after insertion");

   -- Test with special characters in key
   declare
      Special_Key : constant Text := "special!@#$%^&*()";
      Encoded_Key : constant String := Yeison_Utils.Encode(Special_Key);
   begin
      Test_Map.Insert (+Special_Key, +"special chars");
      Assert (Test_Map.Has_Key (Encoded_Key),
              "Has_Key should handle special characters");
   end;

   -- Test with simple ASCII characters only
   Test_Map.Insert (+"UTF8-test", +"utf8 value");
   Assert (Test_Map.Has_Key ("UTF8-test"),
           "Has_Key should handle simple ASCII characters");
end Yeison_12_Tests.Keys;
