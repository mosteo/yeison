with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package Yeison_Utils with Preelaborate is

   --  Miscellanea supporting code of maybe interest elsewhere

   subtype Text is Wide_Wide_String;

   function Encode (T : Text; Output_BOM : Boolean  := False) return String
                    renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode;

   function Decode (T : String) return Text
                    renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode;

   function JSON_Escape (Str : Text) return Text;
   --  Prepare a string for storage in JSON format. Does not add enclosing
   --  quotes!

   function JSON_Quote (Str : Text) return Text
   is ('"' & JSON_Escape (Str) & '"');
   --  JSON_Escape plus the enclosing double quotes.

   function YAML_Double_Quote_Escape (Str : Text) return Text;
   --  Escapes for YAML output in a doubly-quoted string: "example". The string
   --  is quoted even when no escaping is necessary, to avoid confusing cases
   --  like printable strings starting/ending on YAML control characters.

   function Nicer_Real_Image (Img : Text) return Text;
   --  Remove exponential notation when trivially feasible, e.g.:
   --  2.00000000000000E+00 --> 2.0

   -------------------
   -- General_Reals --
   -------------------

   --  JSON doesn't support directly representing non-finite reals, but TOML
   --  and YAML do.

   generic
      type Real is private;
      with function "<" (L, R : Real) return Boolean is <>;
      with function Image (R : Real) return Text is <>;
      with function "-" (R : Real) return Real is <>;
   package General_Reals is

      type Classes is (Finite, Infinite, NaN);

      type General_Real (Class : Classes := Finite)
      is record
         case Class is
            when Finite   => Value    : Real;
            when Infinite => Positive : Boolean;
            when NaN      => null;
         end case;
      end record;

      --------------
      -- New_Real --
      --------------

      function New_Real (Value : Real) return General_Real
      is (Class => Finite, Value => Value);

      ------------------
      -- New_Infinite --
      ------------------

      function New_Infinite (Positive : Boolean) return General_Real
      is (Class => Infinite, Positive => Positive);

      -------------
      -- New_NaN --
      -------------

      function New_NaN return General_Real is (Class => NaN);

      ------------
      -- Negate --
      ------------

      function Negate (R : General_Real) return General_Real
      is (case R.Class is
             when Finite   => New_Real (-R.Value),
             when Infinite => New_Infinite (not R.Positive),
             when NaN      => New_NaN);
      --  Sign flip that preserves the non-finite classes.

      ---------
      -- "<" --
      ---------

      function "<" (L, R : General_Real) return Boolean
      is (if L.Class = R.Class and then L.Class = Finite then
             L < R
          elsif R.Class = Infinite and then L.Class = Finite then
             True
          elsif L.Class = Infinite and then R.Class = Infinite and then
                R.Positive and then not L.Positive
          then
             True
          else
             False);

      -----------
      -- Image --
      -----------
      --  NOTE: specific formats have different NAN/Inf representations, this
      --  is only valid for general non-strict output.
      function Image (Value : General_Real) return Text
      is (if Value.Class = NaN then
             "nan"
          elsif Value.Class = Finite then
             Image (Value.Value)
          elsif Value.Positive then
             "+inf"
          else
             "-inf");

   end General_Reals;

   ---------------
   -- Big_Reals --
   ---------------

   --  The reals instance shared by both Yeison crates. Big_Real is
   --  Long_Long_Float in both, so the instance lives here once.

   function Nicer_Image (R : Long_Long_Float) return Text
   is (Nicer_Real_Image (R'Wide_Wide_Image));

   package Big_Reals is new General_Reals (Long_Long_Float, "<", Nicer_Image);

end Yeison_Utils;
