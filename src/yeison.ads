with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Characters.Conversions;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package Yeison with Preelaborate is

   type Abstract_Value is interface;

   type Integer_Value is new Abstract_Value with record
      Value : Long_Long_Integer range -(2 ** 63) .. +(2 ** 63 - 1);
   end record;

   type UString is new Ada.Strings.Unbounded.Unbounded_String
     with String_Literal => To_UString;

   function To_UString (S : Wide_Wide_String) return UString;

   type String_Value is new Abstract_Value with record
      Value : UString;
   end record
     with String_Literal => To_String_Value;

   function To_String_Value (S : Wide_Wide_String) return String_Value;

   type Collection is tagged private with
     Integer_Literal => To_Integer_Value,
     String_Literal =>  To_String_Value,
     Aggregate => (Empty          => Empty,
                   Add_Named      => Insert,
                   Add_Unnamed    => Append);

   function Empty return Collection;

   procedure Append (This : in out Collection; Value : Collection);

   procedure Insert (This  : in out Collection;
                     Key   : String;
                     Value : Collection);

   function To_Integer_Value (Image : String) return Collection;
   function To_String_Value  (Wide  : Wide_Wide_String) return Collection;

private

   package Value_Holders is new Ada.Containers.Indefinite_Holders
     (Abstract_Value'Class);

   type Collection is new Value_Holders.Holder with null record;

   use Ada.Strings;

   -----------
   -- Empty --
   -----------

   function Empty return Collection
   is (Collection'(Value_Holders.Empty_Holder with null record));

   ----------------------
   -- To_Integer_Value --
   ----------------------

   function To_Integer_Value (Image : String) return Collection
   is (To_Holder
       (Integer_Value'
          (Value => Long_Long_Integer'Value (Image))));

   ---------------------
   -- To_String_Value --
   ---------------------

   function To_String_Value (S : Wide_Wide_String) return String_Value
   is (Value => To_UString (S));

   function To_String_Value  (Wide  : Wide_Wide_String) return Collection
   is (To_Holder (To_String_Value (Wide)));

   ----------------
   -- To_UString --
   ----------------

   function To_UString (S : Wide_Wide_String) return UString
   is (To_Unbounded_String (UTF_Encoding.Wide_Wide_Strings.Encode (S)));

end Yeison;
