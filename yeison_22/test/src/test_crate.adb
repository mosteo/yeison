with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Test_Crate is
begin
   Put_Line ("Crate: " & Crate.Image);
   Put_Line ("Map nested indexing: Crate (""depends-on"") (""aaa"") => "
             & Crate ("depends-on") ("aaa").Image);
end Test_Crate;
