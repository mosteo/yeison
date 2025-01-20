pragma Ada_2012;

with Yeison_Generic;

package Yeison_12 is

   subtype Int is Long_Long_Integer;

   function Identity (L : Int) return Int is (L);

   subtype Real is Long_Long_Float;

   package Impl is new Yeison_Generic (Int, Identity,
                                       Real);

   subtype Any is Impl.Any;
   --  If we make a new type here instead, indexing through references falls
   --  apart. Also, bringing 'Class as a workaround starts to cause trouble
   --  with dynamically tagged required/not required everywhere.

   --  Resurface non-inherited things

   function Invalid return Any renames Impl.Invalid;

   function False return Any renames Impl.False;
   function True  return Any renames Impl.True;

   use all type Impl.Kinds;

   subtype Str is Any with
     Dynamic_Predicate => Str.Kind = Str_Kind;

   subtype Text is Impl.Text;

   function Empty_Map return Any renames Impl.Empty_Map;
   function Empty_Vec return Any renames Impl.Empty_Vec;

   --  Ada 2012-specific helpers

   type Any_Array is array (Positive range <>) of Any;

   function Vec (A : Any_Array) return Any
     with Post => Vec'Result.Kind = Vec_Kind;

   function "+" (I : Int) return Any renames Impl.Make_Int;
   function "+" (S : Text) return Any renames Impl.Make_Str;

end Yeison_12;
