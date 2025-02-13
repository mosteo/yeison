with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Unbounded;

with Interfaces;

package body Yeison_Utils is

   ------------------------
   -- Escape_Unprintable --
   ------------------------

   function Escape_Unprintable (C : Wide_Wide_Character) return Text
   is
      --  Borrowed from GNATCOLL.JSON.Utility

      use Interfaces;

      To_Hex : constant array (Unsigned_16 range 0 .. 15)
        of Wide_Wide_Character := "0123456789ABCDEF";

      Code  : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
      Buf   : Text (1 .. 12);
      Last  : Natural := Buf'First - 1;

      --------------------
      -- Append_Escaped --
      --------------------

      procedure Append_Escaped (Code : Unsigned_16) is
      begin
         Last := Last + 6;
         Buf (Last - 5 .. Last - 4) := "\u";
         Buf (Last - 3) := To_Hex ((Code / 16#1000#) mod 16#10#);
         Buf (Last - 2) := To_Hex ((Code / 16#100#) mod 16#10#);
         Buf (Last - 1) := To_Hex ((Code / 16#10#) mod 16#10#);
         Buf (Last) := To_Hex (Code mod 16#10#);
      end Append_Escaped;

   begin
      if Code <= 16#FFFF# then
         Append_Escaped (Unsigned_16 (Code));

      else
         --  Represent character as surrogate pair

         Append_Escaped
           (16#D800# + Unsigned_16 ((Code - 16#1_0000#) / 16#400#));
         Append_Escaped (16#DC00# + Unsigned_16 (Code mod 16#400#));
      end if;

      return Buf (Buf'First .. Last);
   end Escape_Unprintable;

   -----------------
   -- JSON_Escape --
   -----------------

   function JSON_Escape (Str : Text) return Text is
      use Ada.Characters.Wide_Wide_Latin_1;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String;
   begin
      for Char of Str loop
         --  Taken from GNATCOLL
         case Char is
            when NUL =>
               Append (Result, "\u0000");
            when '"' =>
               Append (Result, "\""");
            when '\' =>
               Append (Result, "\\");
            when BS =>
               Append (Result, "\b");
            when FF =>
               Append (Result, "\f");
            when LF =>
               Append (Result, "\n");
            when CR =>
               Append (Result, "\r");
            when HT =>
               Append (Result, "\t");
            when others =>
               if Wide_Wide_Character'Pos (Char) < 32 then
                  Append (Result, Escape_Unprintable (Char));
               elsif Wide_Wide_Character'Pos (Char) >= 16#80# then
                  Append (Result, Escape_Unprintable (Char));
               else
                  Append (Result, Char);
               end if;
         end case;
      end loop;

      return To_Wide_Wide_String (Result);
   end JSON_Escape;

end Yeison_Utils;
