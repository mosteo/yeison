package Yeison_Utils with Preelaborate is

   --  Miscellanea supporting code of maybe interest elsewhere

   subtype Text is Wide_Wide_String;

   function JSON_Escape (Str : Text) return Text;
   --  Prepare a string for storage in JSON format. Does not add enclosing
   --  quotes!

   --  JSON doesn't support directly representing non-finite reals, but TOML
   --  and YAML do.

   generic
      type Real is private;
      with function "<" (L, R : Real) return Boolean is <>;
      with function Image (R : Real) return Text is <>;
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

end Yeison_Utils;
