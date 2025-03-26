with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Insert_Replace is
   use Operators;

   Test_Map : Any := Empty_Map;
begin
   -- First insert
   Test_Map.Insert (+"key", +"original_value");
   Assert (Test_Map ("key") = "original_value",
           "Initial insert should set the value");

   -- Insert with Replace = False (default) should raise an exception
   -- when the key already exists
   begin
      Test_Map.Insert (+"key", +"new_value", Replace => False);
      Assert (False, "Insert with Replace=False should raise an exception");
   exception
      when others =>
         null; -- Any exception is acceptable
   end;

   -- Insert with Replace = True should change the existing value
   Test_Map.Insert (+"key", +"new_value", Replace => True);
   Assert (Test_Map ("key") = "new_value",
           "Insert with Replace=True should change existing value");
end Yeison_12_Tests.Insert_Replace;
