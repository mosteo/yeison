pragma Ada_2012;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.IO; use GNAT.IO;

with Yeison_12; use Yeison_12;

procedure Test_12 is

   package Yeison renames Yeison_12;

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

   --  function Vec (This : Vec) return Any renames Yeison.Make.Vec;

begin
   Report ("empty", Invalid);

   --  Bool scalars

   Report ("literal bool", True);

   --  Int scalars

   Report ("literal integer", +1);

   --  String scalars

   Report ("literal string", +"asdf");

   declare
      S : constant Any := +"qwerS";
      T : constant Str := +"qwerT";
   begin
      Report ("var string", S);
      Report ("var string", T);
   end;

   --  Maps

   Report ("empty map", Empty_Map);
   Report ("literal map",
           Empty_Map
           .Insert (+"one", +"one")
           .Insert (+"two", +2)
           .Insert (+"three", Vec ((+1, +"two", +3)))
           .Insert (+"four", Empty_Map.Insert (+"4a", +4)));

   declare
      M : Any;
   begin
      M (+"hi") := +"there";
      Report ("map incremental", M);
   end;

   --  Vectors

   Report ("empty vec", Empty_Vec);
   Report ("homo vec", Vec ((+1, +2, +3)));
   Report ("hetero vec", Vec ((+1, +"two", +3)));

   declare
      V : Any;
   begin
      V (+1) := +"one";
   end;

   --  References

   declare
      X : Any;
   begin
      X.Self := +1;
      Report ("X = 1", X);
      X.Self := +"one";
      Report ("X = ""one""", X);
      X.Self := Any'(Empty_Vec);
      Report ("X = {}", X);
      X.Self := Empty_Map;
      Report ("X = {}", X);
      X.Self := Empty_Vec;
      Report ("X = []", X);
   end;

end Test_12;
