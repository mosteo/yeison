pragma Ada_2012;

with Yeison_Generic;

package Yeison_12 is

   subtype Int is Long_Long_Integer;

   function Identity (L : Int) return Int is (L);

   subtype Real is Long_Long_Float;

   package Impl is new Yeison_Generic (Int, Identity, Int'Wide_Wide_Image,
                                       Real, Real'Wide_Wide_Image);

   subtype Any is Impl.Any;
   function "=" (L, R : Any) return Boolean renames Impl."=";
   --  If we make a new type here instead, indexing through references falls
   --  apart. Also, bringing 'Class as a workaround starts to cause trouble
   --  with dynamically tagged required/not required everywhere.

   --  Resurface non-inherited things

   function Invalid return Any renames Impl.Invalid;

   function False return Any renames Impl.False;
   function True  return Any renames Impl.True;

   subtype Kinds is Impl.Kinds;

   subtype Composite_Kinds is Impl.Composite_Kinds;
   use all type Kinds;

   subtype Str is Any with
     Dynamic_Predicate => Str.Kind = Str_Kind;

   subtype Text is Impl.Text;

   function Make_Int (This : Int) return Any renames Impl.Make_Int;
   function Make_Str (This : Text) return Any renames Impl.Make_Str;

   function Empty_Map return Any renames Impl.Empty_Map;
   function Empty_Vec return Any renames Impl.Empty_Vec;

   --  Ada 2012-specific helpers

   type Any_Array is array (Positive range <>) of Any;

   function Vec (A : Any_Array) return Any
     with Post => Vec'Result.Kind = Vec_Kind;

   function "+" (I : Int) return Any renames Impl.Make_Int;
   function "+" (S : Text) return Any renames Impl.Make_Str;

end Yeison_12;
