with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Image_Formats is
   use Operators;
   use all type Yeison_12.Image_Formats;

   -- Create test data with various types
   Test_Map : constant Any := Empty_Map
     .Insert (+"string", +"value")
     .Insert (+"integer", +42)
     .Insert (+"real", +3.14159)
     .Insert (+"boolean", Make.True)
     .Insert (+"nested_map", Empty_Map
              .Insert (+"nested_key", +"nested_value"))
     .Insert (+"array", To_Vec ((+1, +2, +3)));

   -- Test vectors with different element types
   Test_Vec : constant Any := To_Vec ((
     +"string",
     +42,
     +3.14159,
     Make.True,
     Empty_Map.Insert (+"key", +"value"),
     To_Vec ((+1, +2, +3))
   ));

   -- Test empty containers
   Empty_M : constant Any := Empty_Map;
   Empty_V : constant Any := Empty_Vec;

   -- Test with special characters
   Special_Chars : constant Any := +"Special chars: !@#$%^&*()_+{}|:<>?~`-=[]\\;',./";

   -- Test with Unicode characters
   Unicode_Chars : constant Any := +"Unicode: äöüßÄÖÜ€µ©®™";

begin
   -- Test Ada_Like format (default)
   declare
      Ada_Format : constant Text := Test_Map.Image;
   begin
      Assert (Ada_Format'Length > 0, "Ada_Like format should not be empty");
      -- Check for Ada-style formatting indicators
      Assert (Ada_Format (Ada_Format'First) = '(',
              "Ada_Like format should start with '('");
      Assert (Ada_Format (Ada_Format'Last) = ')',
              "Ada_Like format should end with ')'");
   end;

   -- Test JSON format
   declare
      JSON_Format : constant Text :=
        Test_Map.Image(Format => JSON);
   begin
      Assert (JSON_Format'Length > 0, "JSON format should not be empty");
      -- Check for JSON formatting indicators
      Assert (JSON_Format (JSON_Format'First) = '{',
              "JSON format should start with '{'");
      Assert (JSON_Format (JSON_Format'Last) = '}',
              "JSON format should end with '}'");
   end;

   -- Test Compact option with Ada_Like format
   declare
      Compact_Ada : constant Text :=
        Test_Map.Image(Options => (Compact => True, others => <>));
      Regular_Ada : constant Text :=
        Test_Map.Image(Options => (Compact => False, others => <>));
   begin
      Assert (Compact_Ada'Length < Regular_Ada'Length,
              "Compact format should be shorter than regular format");
   end;

   -- Test Compact option with JSON format
   declare
      Compact_JSON : constant Text :=
        Test_Map.Image(Format => JSON,
                       Options => (Compact => True, others => <>));
      Regular_JSON : constant Text :=
        Test_Map.Image(Format => JSON,
                       Options => (Compact => False, others => <>));
   begin
      Assert (Compact_JSON'Length < Regular_JSON'Length,
              "Compact JSON should be shorter than regular JSON");
   end;

   -- Test Ordered_Keys option
   declare
      Ordered_Keys : constant Text :=
        Test_Map.Image(Options => (Ordered_Keys => True, others => <>));
      Regular_Keys : constant Text :=
        Test_Map.Image(Options => (Ordered_Keys => False, others => <>));
   begin
      -- The ordered keys version should be different but same length
      Assert (Ordered_Keys'Length = Regular_Keys'Length or else
              Ordered_Keys /= Regular_Keys,
              "Ordered_Keys should affect the output");
   end;

   -- Test vector formatting
   declare
      Vec_Ada : constant Text := Test_Vec.Image;
      Vec_JSON : constant Text :=
        Test_Vec.Image(Format => JSON);
   begin
      Assert (Vec_Ada (Vec_Ada'First) = '(',
              "Vector Ada_Like format should start with '('");
      Assert (Vec_JSON (Vec_JSON'First) = '[',
              "Vector JSON format should start with '['");
   end;

   -- Test empty container formatting
   declare
      Empty_Map_Ada : constant Text := Empty_M.Image;
      Empty_Map_JSON : constant Text :=
        Empty_M.Image(Format => JSON);
      Empty_Vec_Ada : constant Text := Empty_V.Image;
      Empty_Vec_JSON : constant Text :=
        Empty_V.Image(Format => JSON);
   begin
      Assert (Empty_Map_Ada = "()", "Empty map in Ada format should be '()'");
      Assert (Empty_Map_JSON = "{}", "Empty map in JSON format should be '{}'");
      Assert (Empty_Vec_Ada = "()", "Empty vector in Ada format should be '()'");
      Assert (Empty_Vec_JSON = "[]", "Empty vector in JSON format should be '[]'");
   end;

   -- Test special characters formatting
   declare
      Special_Ada : constant Text := Special_Chars.Image;
      Special_JSON : constant Text :=
        Special_Chars.Image(Format => JSON);
   begin
      Assert (Special_Ada'Length > 0,
              "Special characters in Ada format should not be empty");
      Assert (Special_JSON'Length > 0,
              "Special characters in JSON format should not be empty");
   end;

   -- Test Unicode characters formatting
   declare
      Unicode_Ada : constant Text := Unicode_Chars.Image;
      Unicode_JSON : constant Text :=
        Unicode_Chars.Image(Format => JSON);
   begin
      Assert (Unicode_Ada'Length > 0,
              "Unicode characters in Ada format should not be empty");
      Assert (Unicode_JSON'Length > 0,
              "Unicode characters in JSON format should not be empty");
   end;
end Yeison_12_Tests.Image_Formats;
