with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.IO; use GNAT.IO;

with Yeison; use Yeison;

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

   function Vec (This : Vec) return Any renames Yeison.Make.Vec;

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
      T : constant Str := "qwerT";
   begin
      Report ("var string", S);
      Report ("var string", T);
   end;

   --  Maps

   Report ("empty map", []);
   Report ("literal map",
           ["one" => "one",
            "two" => 2,
            "three" => Vec ([1, "two", 3]),
            "four"  => ["4a" => 4]]);

   declare
      M : Any;
   begin
      M ("hi") := "there";
      Report ("map incremental", M);
   end;

   --  Vectors

   Report ("empty vec", Vec ([]));
   Report ("homo vec", Vec ([1, 2, 3]));
   Report ("hetero vec", Vec ([1, "two", 3]));

   declare
      V : Any;
   begin
      V (1) := "one";
   end;

   --  References

   declare
      X : Any;
   begin
      X.Self := 1;
      Report ("X = 1", X);
      X.Self := "one";
      Report ("X = ""one""", X);
      X.Self := Any'([]);
      Report ("X = {}", X);
      X.Self := Map ([]);
      Report ("X = {}", X);
      X.Self := Vec ([]);
      Report ("X = []", X);
   end;

end Test;
