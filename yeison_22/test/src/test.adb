pragma Ada_2022;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.IO; use GNAT.IO;

with Yeison; use Yeison; use Yeison.Operators;

with Test_Crate; pragma Unreferenced (Test_Crate);
with Test_Indexing;

procedure Test is

   ------------
   -- Report --
   ------------

   procedure Report (Label : String; Value : Yeison.Any) is
   begin
      if Value.Is_Valid then
         Put_Line (Label & " (" & Value.Kind'Image & "):");
      else
         Put_Line (Label & " (INVALID):");
      end if;
      Put_Line (Encode (Value.Image));
      New_Line;
   end Report;

begin
   Report ("empty", Yeison.Invalid);

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

   Report ("empty vec", +[]);
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
      X.As_Ref := Any'([]);
      Report ("X = {}", X);
      X.As_Ref := Map'[];
      Report ("X = {}", X);
      X.As_Ref := Vec'(+[]);
      Report ("X = []", X);
   end;

end Test;
