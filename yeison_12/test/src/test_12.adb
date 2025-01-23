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

   ---------------
   -- Report_RW --
   ---------------

   procedure Report_RW (Label : String; Value : in out Yeison.Any) is
   begin
      Report (Label, Value);
   end Report_RW;

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
           .Insert (+"three", To_Vec ((+1, +"two", +3)))
           .Insert (+"four", Empty_Map.Insert (+"4a", +4)));

   declare
      M : Any;
   begin
      M (+"hi") := +"there";
      Report ("map incremental", M);
   end;

   --  Vectors

   Report ("empty vec", Empty_Vec);
   Report ("homo vec", To_Vec ((+1, +2, +3)));
   Report ("hetero vec", To_Vec ((+1, +"two", +3)));

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

   Report ("constant indexing",
           Empty_Map.Insert (+"key", +"val")
           (+"key"));
   Report_RW ("variable indexing",
              Empty_Map.Insert (+"key", +"val") (+"key"));

   declare
      M : constant Any := Empty_Map.Insert (+"key", +"val");
   begin
      Report ("map on the fly", M);
      Report ("map indexing", M (+"key"));
      pragma Assert (M (+"key").As_Text = "val");
   end;

   pragma Assert
     (Empty_Map.Insert (+"key", +"val") (+"key").As_Text = "val");

end Test_12;
