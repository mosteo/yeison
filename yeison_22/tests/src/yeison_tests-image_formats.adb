pragma Ada_2022;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

procedure Yeison_Tests.Image_Formats is

   function "+" (T : Text) return String is (Encode (T));

   M : constant Any := ["one" => 1];
   V : constant Any := +[1];
   S : constant Any := "hi";
begin
   --  Scalars
   Assert (+Any'(1).Image = "1",            "int image");
   Assert (+S.Image (JSON) = """hi""",      "json string image is quoted");
   Assert (+True.Image (JSON) = "true",     "json bool image");

   --  Empty containers
   Assert (+Empty_Map.Image (JSON) = "{}",  "empty map json");
   Assert (+Empty_Vec.Image (JSON) = "[]",  "empty vec json");

   --  Compact one-element rendering
   Assert (+M.Image (JSON, (Compact => True, others => <>)) = "{ ""one"": 1 }",
           "compact json map");
   Assert (+V.Image (JSON, (Compact => True, others => <>)) = "[ 1 ]",
           "compact json vec");
end Yeison_Tests.Image_Formats;
