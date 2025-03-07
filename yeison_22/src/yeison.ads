pragma Ada_2022;

private with Ada.Characters.Conversions;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;

with Yeison_Generic;

package Yeison with Preelaborate is

   ---------------------
   --  Preliminaries  --
   ---------------------

   --  These enable the instantiation below of Any; can be skipped. They
   --  cannot be private as we want to visibly inherit lots of operations
   --  from Yeison_Generic.

   use Ada.Numerics.Big_Numbers;

   package Bigint_Conversions is
     new Big_Integers.Signed_Conversions (Long_Long_Integer);

   function Image (I : Big_Integers.Big_Integer) return Wide_Wide_String;
   function Image (R : Big_Reals.Big_Real) return Wide_Wide_String;

   package Impl is
     new Yeison_Generic (Big_Integers.Big_Integer,
                         Bigint_Conversions.From_Big_Integer,
                         Image,

                         Big_Reals.Big_Real,
                         Image,

                         Big_Integers."<",
                         Big_Reals."<");

   use all type Impl.Kinds;

   subtype Scalar_Kinds is Impl.Scalar_Kinds;

   subtype Composite_Kinds is Impl.Composite_Kinds;

   subtype Text is Impl.Text;

   -----------
   --  Any  --
   -----------

   type Any is new Impl.Any with private with
     Aggregate => (Empty     => Empty_Map,
                   Add_Named => Insert),
     Integer_Literal   => To_Int,
     Real_Literal      => To_Real,
     String_Literal    => To_Str,
     Constant_Indexing => Const_Ref,
     Variable_Indexing => Reference;
   --  We need a new derived type because user literal aspects cannot be
   --  applied to subtypes (drats).

   --  Check Yeison_Generic spec for the full features of the type that are
   --  inherited here.

   --  These are problematic, as declaring using them doesn't really assign
   --  the expected kind to the variable, and for maps and vectors it results
   --  in spurious invalid initialization. Use at your own risk, they'll be
   --  probably deprecated at some point...

   subtype Bool is Any with Dynamic_Predicate => Bool.Kind = Bool_Kind;
   subtype Int is Any with Dynamic_Predicate => Int.Kind = Int_Kind;
   subtype Map is Any with
     Dynamic_Predicate => Map.Kind in Nil_Kind | Map_Kind;
   subtype Real is Any with Dynamic_Predicate => Real.Kind = Real_Kind;
   subtype Str is Any with Dynamic_Predicate => Str.Kind = Str_Kind;
   subtype Vec is Any with
     Dynamic_Predicate => Vec.Kind in Nil_Kind | Vec_Kind;

   function Nil return Any;
   function False return Any;
   function True return Any;

   ---------------
   --  Scalars  --
   ---------------

   function To_Int  (Img : String) return Any;
   function To_Real (Img : String) return Any;
   function To_Str  (Img : Text) return Any;

   -----------
   --  Map  --
   -----------

   --  Minimal facilities to enable aspects. Full operations in Yeison_Generic

   procedure Insert (This  : in out Any;
                     Key   : Text;
                     Value : Any);

   ----------------
   --  Indexing  --
   ----------------

   function Get (This, Pos : Any) return Any with
     Pre => This.Kind in Composite_Kinds
     and then Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Note this always returns a copy; for in place modification use Ref

   --  References and the like for indexing. Not really directly interesting.

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   function As_Ref (This : aliased Any) return Ref;
   --  Not really needed by clients; used in tests

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

   ---------------
   -- Operators --
   ---------------

   --  Cannot be instantiated here as Any must be private. Simply with and use
   --  Yeison.Operators.

   function To_Any (This : Impl.Any) return Any;
   --  This should be private but it must be publicly visible by the nested
   --  Operators. Another spill-over of trying to reuse for 12/22 versions.

private

   Unimplemented : exception;

   type Any is new Impl.Any with null record;
   --  Must be private to avoid error with [] getting confused

   package Charconv renames Ada.Characters.Conversions;

   -----------
   -- Image --
   -----------

   function Image (I : Big_Integers.Big_Integer) return Wide_Wide_String
   is (Charconv.To_Wide_Wide_String (Big_Integers.To_String (I)));

   -----------
   -- Image --
   -----------

   function Image (R : Big_Reals.Big_Real) return Wide_Wide_String
   is (Charconv.To_Wide_Wide_String (Big_Reals.To_String (R)));

end Yeison;
