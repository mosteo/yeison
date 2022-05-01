with Ada.Text_IO; use Ada.Text_IO;

package body Yeison_Profiles is

   ------------
   -- Append --
   ------------

   procedure Append (L : in out List; I : Integer) is
   begin
      Put_Line ("Append Integer " & I'Image);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (L : in out List; S : String) is
   begin
      Put_Line ("Append String " & S);
   end Append;

end Yeison_Profiles;
