with Yeison_12; use Yeison_12;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Characters.Conversions;
use  Ada.Characters.Conversions;

procedure Yeison_12_Tests.Character_Encodings is
   use Operators;

   -- Test strings with various character sets
   ASCII_Only    : constant Any := +"ASCII only: Hello, World!";
   Latin_1_Chars : constant Any := +"Latin-1: éèêëàáâäôöòóùúûüÿçñ";
   Unicode_Chars : constant Any := +"Unicode: äöüßÄÖÜ€µ©®™";
   Mixed_Chars   : constant Any := +"Mixed: Hello, 世界!";
   Empty_Str     : constant Any := +"";

begin
   -- Test As_UTF_8 with ASCII-only string
   declare
      UTF8_Result : constant String := ASCII_Only.As_UTF_8;
   begin
      Assert (UTF8_Result = "ASCII only: Hello, World!",
              "As_UTF_8 should correctly convert ASCII-only string");
   end;

   -- Test As_Latin_1 with ASCII-only string
   declare
      Latin1_Result : constant String := ASCII_Only.As_Latin_1;
   begin
      Assert (Latin1_Result = "ASCII only: Hello, World!",
              "As_Latin_1 should correctly convert ASCII-only string");
   end;

   -- Test As_UTF_8 with Latin-1 characters
   declare
      UTF8_Result : constant String := Latin_1_Chars.As_UTF_8;
      -- Convert back to Wide_Wide_String to compare
      Roundtrip : constant Wide_Wide_String := Decode (UTF8_Result);
   begin
      Assert (Roundtrip = Latin_1_Chars.As_Text,
              "As_UTF_8 should correctly handle Latin-1 characters");
   end;

   -- Test As_Latin_1 with Latin-1 characters
   declare
      Latin1_Result : constant String := Latin_1_Chars.As_Latin_1;
      -- Convert back to Wide_Wide_String to compare
      Roundtrip : constant Wide_Wide_String := To_Wide_Wide_String (Latin1_Result);
   begin
      Assert (Roundtrip = Latin_1_Chars.As_Text,
              "As_Latin_1 should correctly handle Latin-1 characters");
   end;

   -- Test As_UTF_8 with Unicode characters
   declare
      UTF8_Result : constant String := Unicode_Chars.As_UTF_8;
      -- Convert back to Wide_Wide_String to compare
      Roundtrip : constant Wide_Wide_String := Decode (UTF8_Result);
   begin
      Assert (Roundtrip = Unicode_Chars.As_Text,
              "As_UTF_8 should correctly handle Unicode characters");
   end;

   -- Test As_UTF_8 with mixed characters
   declare
      UTF8_Result : constant String := Mixed_Chars.As_UTF_8;
      -- Convert back to Wide_Wide_String to compare
      Roundtrip : constant Wide_Wide_String := Decode (UTF8_Result);
   begin
      Assert (Roundtrip = Mixed_Chars.As_Text,
              "As_UTF_8 should correctly handle mixed characters");
   end;

   -- Test As_UTF_8 with empty string
   declare
      UTF8_Result : constant String := Empty_Str.As_UTF_8;
   begin
      Assert (UTF8_Result'Length = 0,
              "As_UTF_8 should return empty string for empty input");
   end;

   -- Test As_Latin_1 with empty string
   declare
      Latin1_Result : constant String := Empty_Str.As_Latin_1;
   begin
      Assert (Latin1_Result'Length = 0,
              "As_Latin_1 should return empty string for empty input");
   end;

   -- Test with very long string
   declare
      Long_String : Text (1 .. 1000);
      Long_Any : Any;
   begin
      -- Fill with repeating pattern
      for I in Long_String'Range loop
         Long_String (I) := 'A';
      end loop;

      Long_Any := +Long_String;

      declare
         UTF8_Result : constant String := Long_Any.As_UTF_8;
         Latin1_Result : constant String := Long_Any.As_Latin_1;
         UTF8_Roundtrip : constant Wide_Wide_String := Decode (UTF8_Result);
         Latin1_Roundtrip : constant Wide_Wide_String := To_Wide_Wide_String (Latin1_Result);
      begin
         Assert (UTF8_Result'Length = 1000,
                 "As_UTF_8 should handle long strings");
         Assert (Latin1_Result'Length = 1000,
                 "As_Latin_1 should handle long strings");
         Assert (UTF8_Roundtrip = Long_Any.As_Text,
                 "As_UTF_8 roundtrip should preserve the string");
         Assert (Latin1_Roundtrip = Long_Any.As_Text,
                 "As_Latin_1 roundtrip should preserve the string");
      end;
   end;

   -- Test with string containing control characters
   declare
      Tab : constant Wide_Wide_Character := Wide_Wide_Character'Val(9);
      LF : constant Wide_Wide_Character := Wide_Wide_Character'Val(10);
      CR : constant Wide_Wide_Character := Wide_Wide_Character'Val(13);
      NUL : constant Wide_Wide_Character := Wide_Wide_Character'Val(0);
      Control_Text : constant Wide_Wide_String :=
        "Control chars: " & Tab & LF & CR & NUL & " end";
      Control_Chars : constant Any := +Control_Text;

      UTF8_Result : constant String := Control_Chars.As_UTF_8;
      -- Convert back to Wide_Wide_String to compare
      Roundtrip : constant Wide_Wide_String := Decode (UTF8_Result);
   begin
      Assert (UTF8_Result'Length > 0,
              "As_UTF_8 should handle control characters");
      Assert
        (Roundtrip = Control_Text,
         "As_UTF_8 should correctly handle control characters");
   end;
end Yeison_12_Tests.Character_Encodings;
