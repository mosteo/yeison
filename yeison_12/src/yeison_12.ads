pragma Ada_2012;

with Yeison_Generic;
with Yeison_Utils;

package Yeison_12 with Preelaborate is

   subtype Big_Int is Long_Long_Integer;

   function Identity (L : Big_Int) return Big_Int is (L);

   subtype Big_Real is Long_Long_Float;

   function Nicer_Image (R : Big_Real) return Wide_Wide_String;
   --  Avoid scientific notation when easy to do so

   package Reals is new Yeison_Utils.General_Reals (Big_Real,
                                                    "<",
                                                    Nicer_Image);

   package Impl is
     new Yeison_Generic (Big_Int, Identity, Big_Int'Wide_Wide_Image,
                         Reals.General_Real, Reals.Image,
                         "<", Reals."<");

   type Any is new Impl.Any with null record with
     --  Constant_Indexing => Const_Ref,
     Variable_Indexing => Reference;
   --  Enabling constant indexing limits how we can use indexing in transient
   --  expressions. Not sure this is entirely a good idea...

   subtype Scalar is Impl.Scalar;

   package Scalars renames Impl.Scalars;

   subtype Bool is Any with Dynamic_Predicate => Bool.Kind = Bool_Kind;
   subtype Int  is Any with Dynamic_Predicate => Int.Kind = Int_Kind;
   --  subtype Map  is Any with Dynamic_Predicate => Map.Kind = Map_Kind;
   --  Triggers bug in GNAT 10
   subtype Real is Any with Dynamic_Predicate => Real.Kind = Real_Kind;
   subtype Str  is Any with Dynamic_Predicate => Str.Kind = Str_Kind;
   subtype Vec  is Any with Dynamic_Predicate => Vec.Kind = Vec_Kind;

   function To_Any (This : Impl.Any) return Any;

   --  Resurface non-inherited things

   subtype Kinds is Impl.Kinds;

   subtype Scalar_Kinds is Impl.Scalar_Kinds;
   subtype Composite_Kinds is Impl.Composite_Kinds;
   use all type Kinds;

   subtype Text is Impl.Text;

   ----------------
   --  Indexing  --
   ----------------

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   --  We need to recreate references for the access discriminant to use the
   --  proper type...

   function Const_Ref (This : Any; Pos : Any) return Const with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  See notes on Reference below. Same applies, except for the
   --  initialization of empty maps/vectors.

   function Reference (This : Any; Pos : Any) return Ref with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Any may be a scalar, which will be used as key/index, or a vector that
   --  will be consumed one element at a time. In YAML, keys can be complex
   --  types, which is discouraged, and this is explicitly not supported.
   --
   --  If This is invalid, the appropriate holder value will be created (vec or
   --  map) depending on Any.Kind being Int or something else. If you want to
   --  force either one, assign first an empty value.

   function Self (This : aliased Any) return Ref;

   ---------------
   -- Operators --
   ---------------

   package Operators is

      package Impl is new Yeison_12.Impl.Operators (Any);

      function "+" (This : Big_Int) return Any renames Impl.Make.Int;
      function "+" (This : Text) return Any renames Impl.Make.Str;
      function To_Vec (This : Impl.Any_Array) return Any renames Impl.Vec;

   end Operators;

   ----------
   -- Make --
   ----------

   package Make renames Operators.Impl.Make;

private

   Unimplemented : exception;

   -----------------
   -- Nicer_Image --
   -----------------

   function Nicer_Image (R : Big_Real) return Wide_Wide_String
   is (Yeison_Utils.Nicer_Real_Image (R'Wide_Wide_Image));

end Yeison_12;
