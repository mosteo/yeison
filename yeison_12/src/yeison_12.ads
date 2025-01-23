pragma Ada_2012;

with Yeison_Generic;

package Yeison_12 is

   subtype Big_Int is Long_Long_Integer;

   function Identity (L : Big_Int) return Big_Int is (L);

   subtype Big_Real is Long_Long_Float;

   package Impl is
     new Yeison_Generic (Big_Int, Identity, Big_Int'Wide_Wide_Image,
                         Big_Real, Big_Real'Wide_Wide_Image);

   type Any is new Impl.Any with null record with
     Constant_Indexing => Const_Ref,
     Variable_Indexing => Reference;

   subtype Bool is Any with Dynamic_Predicate => Bool.Kind = Bool_Kind;
   subtype Int  is Any with Dynamic_Predicate => Int.Kind = Int_Kind;
   subtype Map  is Any with Dynamic_Predicate => Map.Kind = Map_Kind;
   subtype Real is Any with Dynamic_Predicate => Real.Kind = Real_Kind;
   subtype Str  is Any with Dynamic_Predicate => Str.Kind = Str_Kind;
   subtype Vec  is Any with Dynamic_Predicate => Vec.Kind = Vec_Kind;

   --  Resurface non-inherited things

   subtype Kinds is Impl.Kinds;

   subtype Scalar_Kinds is Impl.Scalar_Kinds;
   subtype Composite_Kinds is Impl.Composite_Kinds;
   use all type Kinds;

   subtype Text is Impl.Text;

   package Operators renames Impl.Operators;

   --  Ada 2012-specific helpers

   type Any_Array is array (Positive range <>) of Any;

   function To_Vec (A : Any_Array) return Any
     with Post => To_Vec'Result.Kind = Vec_Kind;

   function "+" (I : Big_Int) return Any renames Make_Int;
   function "+" (S : Text) return Any renames Make_Str;

   ----------------
   --  Indexing  --
   ----------------

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   --  We need to recreate references for the access discriminant to use the
   --  proper type...

   function Const_Ref (This : aliased Any; Pos : Any) return Const with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  See notes on Reference below. Same applies, except for the
   --  initialization of empty maps/vectors.

   function Reference (This : aliased Any; Pos : Any) return Ref with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Any may be a scalar, which will be used as key/index, or a vector that
   --  will be consumed one element at a time. In YAML, keys can be complex
   --  types, which is discouraged, and this is explicitly not supported.
   --
   --  If This is invalid, the appropriate holder value will be created (vec or
   --  map) depending on Any.Kind being Int or something else. If you want to
   --  force either one, assign first an empty value.

   function Self (This : aliased Any) return Ref;

end Yeison_12;
