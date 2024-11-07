with GNAT.IO; use GNAT.IO;

with Yeison; use Yeison;

procedure Test is

   ------------
   -- Report --
   ------------

   procedure Report (Label : String; Value : Yeison.Any'Class) is
   begin
      Put_Line (Label);
      Put_Line (Value'Image);
      New_Line;
   end Report;

begin
   Report ("empty", Yeison.Invalid);

   --  String scalars

   Report ("literal string", "asdf");

   declare
      S : constant Any := "qwerS";
      T : constant Str := "qwerT";
   begin
      Report ("var string", S);
      Report ("var string", T);
   end;

end Test;
