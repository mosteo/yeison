with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Unbounded;

with Interfaces;

package body Yeison_Utils is

   function Nicer_Real_Image (Img : Text) return Text is
      Last : Natural := Img'Last;
   begin
      if Img'Length > 4 and then Img (Last - 3 .. Last) = "E+00" then
         Last := Last - 4;

         --  Remove zeroes at the end, but keep one after the '.'

         while Img (Last) = '0' and then
               Last - 1 in Img'Range and then Img (Last - 1) /= '.'
         loop
            Last := Last - 1;
         end loop;
      end if;

      return Img (Img'First .. Last);
   end Nicer_Real_Image;

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

   ------------------------------
   -- YAML_Double_Quote_Escape --
   ------------------------------

   function YAML_Double_Quote_Escape (Str : Text) return Text is
      subtype WWChar is Wide_Wide_Character;
      use Ada.Characters.Wide_Wide_Latin_1;
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String;

      Hex : constant array (Interfaces.Unsigned_32 range 0 .. 15)
        of Wide_Wide_Character := "0123456789ABCDEF";

      ------------
      -- To_Hex --
      ------------
      --  Returns the shortest 1-byte, 2-byte or 4-byte sequence
      function To_Hex (Char : WWChar) return Text is
         use Interfaces;
         Code : constant Unsigned_32 := WWChar'Pos (Char);
         B    : constant := 16#10#;
      begin
         if Code <= 16#FF# then
            return
              Hex ((Code / B) mod B) &
              Hex (Code       mod B);
         elsif Code <= 16#FFFF# then
            return
              Hex ((Code / B / B / B) mod B) &
              Hex ((Code / B / B)     mod B) &
              Hex ((Code / B)         mod B) &
              Hex (Code               mod B);
         else
            return
              Hex ((Code / B / B / B / B / B / B / B) mod B) &
              Hex ((Code / B / B / B / B / B / B)     mod B) &
              Hex ((Code / B / B / B / B / B)         mod B) &
              Hex ((Code / B / B / B / B)             mod B) &
              Hex ((Code / B / B / B)                 mod B) &
              Hex ((Code / B / B)                     mod B) &
              Hex ((Code / B)                         mod B) &
              Hex (Code                               mod B);
         end if;
      end To_Hex;

   begin
      Append (Result, '"');

      for Char of Str loop
         case Char is
            --  Non-printable
            when NUL =>
               Append (Result, "\x00");
            when BEL =>
               Append (Result, "\a");
            when BS =>
               Append (Result, "\b");
            when HT =>
               Append (Result, "\t");
            when LF =>
               Append (Result, "\n");
            when VT =>
               Append (Result, "\v");
            when FF =>
               Append (Result, "\f");
            when CR =>
               Append (Result, "\r");
            when ESC =>
               Append (Result, "\e");
            when NEL =>                   -- Unicode next line
               Append (Result, "\N");
            when WWChar'Val (16#2028#) => -- Unicode line separator
               Append (Result, "\L");
            when WWChar'Val (16#2029#) => -- Unicode paragraph separator
               Append (Result, "\L");

            --  Printable but forbidden as-is
            when '"' =>
               Append (Result, "\""");
            when '\' =>
               Append (Result, "\\");

            when others =>
               case Char is
                  --  Printable as-is (ASCII x20-x7E)
                  when WWChar'Val (16#20#) .. WWChar'Val (16#7E#) =>
                     Append (Result, Char);

                  --  \uXXXX
                  when WWChar'Val (16#100#) .. WWChar'Val (16#FFFF#) =>
                     Append (Result, "\u" & To_Hex (Char));

                  --  \UXXXXXXXX
                  when WWChar'Val (16#10000#) .. WWChar'Last =>
                     Append (Result, "\U" & To_Hex (Char));

                  --  \xXX
                  when others =>
                     Append (Result, "\x" & To_Hex (Char));
               end case;
         end case;
      end loop;

      Append (Result, '"');
      return To_Wide_Wide_String (Result);
   end YAML_Double_Quote_Escape;

end Yeison_Utils;
