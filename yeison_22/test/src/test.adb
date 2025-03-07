pragma Ada_2022;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.IO; use GNAT.IO;

with Yeison.Operators; use Yeison; use Yeison.Operators;

--  with Test_Crate; pragma Unreferenced (Test_Crate);
with Test_Indexing;

procedure Test is

   ------------
   -- Report --
   ------------

   procedure Report (Label : String; Value : Yeison.Any) is
   begin
      Put_Line (Label & " (" & Value.Kind'Image & "):");
      Put_Line (Encode (Value.Image));
      New_Line;
   end Report;

   ---------------
   -- Report_RW --
   ---------------

   procedure Report_RW (Label : String; Value : in out Yeison.Any) is
   begin
      Report (Label, Value);
   end Report_RW;

begin
   Report ("empty", Make.Nil);

   --  Bool scalars

   Report ("literal bool", True);

   --  Int scalars

   Report ("literal integer", 1);

   --  String scalars

   Report ("literal string", "asdf");

   declare
      S : constant Any := "qwerS";
   begin
      Report ("var string", S);
   end;

   --  Maps

   Report ("empty map", []);
   Report ("simple map", ["one" => 1]);
   Report ("literal map",
           ["one" => "one",
            "two" => 2,
            "three" => +[1, "two", 3],
            "four"  => ["4a" => 4]]);

   declare
      M : Any;
   begin
      M ("hi") := "there";
      Report ("map incremental", M);
   end;

   --  Vectors

   Report ("empty vec", []);
   Report ("homo vec", +[1, 2, 3]);
   Report ("hetero vec", +[1, "two", 3]);

   declare
      V : Any;
   begin
      V (1) := "one";
      Report ("initialized vec", V);
      V (2) := "two";
      V (1) := 1;
      Report ("modified vec", V);
   end;

   --  References

   declare
      X : Any;
   begin
      X.As_Ref := 1;
      Report ("X = 1", X);
      X.As_Ref := "one";
      Report ("X = ""one""", X);
      X.As_Ref := Any'[];
      Report ("X = {}", X);
      Report ("bug?", Map'[]);
      X.As_Ref := Map'[];
      Report ("X = {}", X);
      X.As_Ref := Empty_Vec; -- Vec'[] results in an invalid value because ???
      Report ("X = []", X);
   end;

   Report ("constant indexing",
           Any'["key" => "val"] ("key"));
   declare
      pragma Warnings (Off); -- Spurious could be constant (?)
      M : Any := Empty_Map.Insert ("key", "val");
      pragma Warnings (On);
   begin
      Report_RW ("variable indexing", M ("key"));
   end;

   declare
      M : constant Any := ["key" => "val"];
   begin
      Report ("map on the fly", M);
      Report ("map indexing", M ("key"));
      pragma Assert (M ("key").As_Text = "val");
   end;

   pragma Assert
     (Any'["key" => "val"] ("key").As_Text = "val");

end Test;
