pragma Ada_2012;

with Ada.Finalization;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Wide_Wide_Unbounded;

generic
   --  Because big numbers aren't available in Ada 2012, and no other
   --  convenient implementation is available that I know, we just delegate
   --  this impl to clients. This limits us currently to Long_Long_Integer
   --  as the max length of vectors.
   type Int_Type is private; -- not range <> to allow Big_Integer
   with function To_Integer (I : Int_Type) return Long_Long_Integer;
   with function Image (I : Int_Type) return Wide_Wide_String is <>;

   type Real_Type is private; -- same about digits <>
   with function Image (R : Real_Type) return Wide_Wide_String is <>;

   with function "<" (L, R : Int_Type) return Boolean is <>;
   with function "<" (L, R : Real_Type) return Boolean is <>;
package Yeison_Generic with Preelaborate is

   subtype Universal_Integer is Long_Long_Integer;

   type Kinds is (Nil_Kind,
                  --  Uninitialized or explicitly null value

                  Bool_Kind,
                  Int_Kind,
                  Real_Kind,
                  Str_Kind,
                  --  Scalar kinds; a single value

                  Map_Kind,
                  Vec_Kind
                  --  Composite kinds; a collection of elements
                 );

   subtype Scalar_Kinds
     is Kinds range Kinds'Succ (Nil_Kind) .. Kinds'Pred (Map_Kind);

   subtype Composite_Kinds is Kinds range Map_Kind .. Kinds'Last;

   subtype Nonscalar_Kinds is Kinds with
     Static_Predicate => Nonscalar_Kinds in Nil_Kind | Composite_Kinds;

   subtype Text is Wide_Wide_String;

   type Any is new Ada.Finalization.Controlled with private;
   --  No other aspects here. This forces us to expose them in Yeison_12 and
   --  Yeison_22, which involves some duplication, but otherwise there are
   --  some ambiguities that rain on our parade...

   --  TODO: remove tagged once GNAT accepts dot notation for all private types

   function "=" (L, R : Any) return Boolean;
   function "=" (L : Any; R : Text) return Boolean;

   function "<" (L, R : Any) return Boolean;

   function Precedes (L, R : Any) return Boolean renames "<";

   --------------
   --  Common  --
   --------------

   type Image_Formats is (Ada_Like, JSON);

   type Image_Options is record
      Compact      : Boolean := False;
      Ordered_Keys : Boolean := False;
   end record;

   function Image (This    : Any'Class;
                   Format  : Image_Formats := Ada_Like;
                   Options : Image_Options := (others => <>))
                   return Text;

   function Has_Value (This : Any) return Boolean with
     Post => Has_Value'Result = (This.Kind /= Nil_Kind);

   function Kind (This : Any) return Kinds;

   type Any_Array is array (Positive range <>) of Any;

   -----------
   --  Nil  --
   -----------

   function Is_Nil (This : Any) return Boolean is (This.Kind = Nil_Kind);

   ---------------
   --  Scalars  --
   ---------------

   --  Separate type to ease initializations elsewhere

   type Scalar (<>) is tagged private;

   function Kind (This : Scalar) return Scalar_Kinds;

   function As_Boolean (This : Scalar) return Boolean;
   function As_Integer (This : Scalar) return Int_Type;
   function As_Real (This : Scalar) return Real_Type;
   function As_Text (This : Scalar) return Text;

   --  See package Scalars below for initializations

   --  Retrieval

   function As_Scalar (This : Any'Class) return Scalar
     with Pre => This.Kind in Scalar_Kinds;

   function As_Bool (This : Any) return Boolean
     with Pre => This.Kind = Bool_Kind;

   function As_Int (This : Any) return Int_Type
     with Pre => This.Kind = Int_Kind;

   function As_Real (This : Any) return Real_Type
     with Pre => This.Kind = Real_Kind;

   function As_Text (This : Any) return Text
     with Pre => This.Kind = Str_Kind;

   function As_UTF_8 (This : Any) return String
     with Pre => This.Kind = Str_Kind;

   function As_Latin_1 (This : Any) return String
     with Pre => This.Kind = Str_Kind;

   --  Overloaded renamings
   function Get (This : Any'Class) return Scalar    renames As_Scalar;
   function Get (This : Any) return Boolean         renames As_Bool;
   function Get (This : Any) return Int_Type        renames As_Int;
   function Get (This : Any) return Real_Type       renames As_Real;
   function Get (This : Any) return Text            renames As_Text;

   --  See package Make below for initializations

   -------------------
   --  Collections  --
   -------------------

   function Is_Empty (This : Any) return Boolean with
     Pre => This.Is_Nil or else This.Kind in Composite_Kinds;

   function Length (This : Any) return Universal_Integer;

   ------------
   --  Maps  --
   ------------

   function Empty_Map return Any
     with Post => Empty_Map'Result.Kind = Map_Kind;

   --  Note that JSON/TOML only accept string keys, but YAML accepts
   --  Any(thing). Meanwhile, Ada 2022 doesn't let you use a non-static value
   --  for keys/indices, so any non-basic type won't do here. Not sure if that
   --  is worth reporting, as this is the subject of active discussion that no
   --  overlapping values should be able to be given, and relaxations on this
   --  point are expressly frowned upon. In conclusion: to initialize with
   --  heterogeneous types, you can't use initialization expressions.

   procedure Insert (This    : in out Any;
                     Key     : Any;
                     Value   : Any;
                     Replace : Boolean := False);

   function Map (This : Any) return Any is (This) with Inline;
   --  A pass-through to help with disambiguation and estetics

   --  Old-style helpers

   function Insert (This    : Any;
                    Key     : Any;
                    Value   : Any;
                    Replace : Boolean := False)
                    return Any;
   --  Returns a copy with the new inserted value

   ---------------
   --  Vectors  --
   ---------------

   procedure Append (This : in out Any; Elem : Any) with
     Pre => This.Kind = Vec_Kind;

   function Append (This : Any; Elem : Any) return Any with
     Pre => This.Kind = Vec_Kind;

   function Empty_Vec return Any;

   function First_Index (This : Any) return Universal_Integer with
     Pre => This.Kind = Vec_Kind;

   function Last_Index (This : Any) return Universal_Integer with
     Pre => This.Kind = Vec_Kind;

   -------------
   -- Scalars --
   -------------

   package Scalars is

      function New_Bool (Val : Boolean)   return Scalar;
      function New_Int  (Val : Int_Type)  return Scalar;
      function New_Real (Val : Real_Type) return Scalar;
      function New_Text (Val : Text)      return Scalar;

   end Scalars;

   subtype Base_Any is Any;

   ----------------
   -- References --
   ----------------
   --  This unwanted complexity is caused by wanting to reuse logic in the
   --  derived types for both the 2012 and 2022 versions. It really is not
   --  worth the effort, lesson learned.
   generic
      type Any is new Yeison_Generic.Any with private;
      with function To_Any (This : Yeison_Generic.Any) return Any is <>;
      --  To avoid mistypes
   package References is

      --  We don't want these to be primitive as that causes ambiguities in
      --  the derived types usage.

      type Ref is access Any;

      function Reference (This : Any; Pos : Any) return Ref with
        Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
      --  Any may be a scalar, which will be used as key/index, or a vector
      --  that will be consumed one element at a time. In YAML, keys can be
      --  complex types, which is discouraged, and this is explicitly not
      --  supported.
      --
      --  If This is nil, the appropriate holder value will be created (vec
      --  or map) depending on Any.Kind being Int or something else. If you
      --  want to force either one, assign first an empty value.

      ----------------
      --  Indexing  --
      ----------------

      function Get (This : Any; Pos : Any) return Any with
        Pre => This.Kind in Composite_Kinds
        and then Pos.Kind in Scalar_Kinds | Vec_Kind;
      --  Note this always returns a copy; for in place modification use Ref

      ------------
      --  Maps  --
      ------------

      function Has_Key (This : Any; Key : Any) return Boolean with
        Pre => This.Kind = Map_Kind;

      function Keys (This : Any; Ordered : Boolean := False) return Any with
        Pre  => This.Kind = Map_Kind,
        Post => Keys'Result.Kind = Vec_Kind;

      ---------------
      --  Vectors  --
      ---------------

      function Head (This : Any) return Any with
        Pre => This.Kind = Vec_Kind and then not This.Is_Empty;
      --  Taking the head of an empty vector is an error

      function Tail (This : Any) return Any with
        Pre => This.Kind = Vec_Kind and then not This.Is_Empty,
        Post => Tail'Result.Length = This.Length - 1;
      --  Taking the tail of an empty vector is an error

   end References;

   -----------------
   --  Iterators  --
   -----------------

   type Cursor (<>) is private;

   generic
      type Any is new Yeison_Generic.Any with private;
      with function To_Any (This : Yeison_Generic.Any) return Any is <>;
   package Iterators is

      function First_Cursor (Container : Any) return Cursor;
      function Next_Cursor (Container  : Any; Pos : Cursor) return Cursor;
      function Has_Element (Container  : Any; Pos : Cursor) return Boolean;
      function Element (Container : Any; Pos : Cursor) return Any;

   end Iterators;

private

   Unimplemented : exception;

   --  package Big_Ints renames Ada.Numerics.Big_Numbers.Big_Integers;
   --  subtype Big_Int is Big_Ints.Big_Integer;
   --
   --  package Big_Reals renames Ada.Numerics.Big_Numbers.Big_Reals;
   --  subtype Big_Real is Big_Reals.Big_Real;

   package WWUStrings renames Ada.Strings.Wide_Wide_Unbounded;
   subtype WWUString is WWUStrings.Unbounded_Wide_Wide_String;

   function U (S : Wide_Wide_String) return WWUString renames
     Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   function S (U : WWUString) return Text renames
     Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   type Any_Impl;

   type Any_Impl_Ptr is access Any_Impl;
   --  Implementation is in body, so we can have a self-referential type but
   --  also to control assignments via Controlled (when assigning through
   --  indexing).

   function Nil_Impl return Any_Impl_Ptr;

   type Any is new Ada.Finalization.Controlled with record
      Impl : Any_Impl_Ptr := Nil_Impl;
   end record with
     Type_Invariant => Impl /= null;

   overriding procedure Adjust (This : in out Any);

   overriding procedure Finalize (This : in out Any);

   type Vec is record
      Vec : Any;
   end record;

   --  These could go in the body if not because of
   --  https://forum.ada-lang.io/t/bug-or-legit-instantiation-in-body-of-
   --  preelaborable-generic-complains-about-non-static-constant/1742

   package Any_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Any'Class, Any'Class,
                                                 "<" => Precedes);

   subtype Universal_Positive is
     Universal_Integer range 1 .. Universal_Integer'Last;

   package Any_Vecs is
     new Ada.Containers.Indefinite_Vectors (Universal_Positive, Any'Class);

   ---------------
   --  Scalars  --
   ---------------

   type Scalar_Data (Kind : Scalar_Kinds := Bool_Kind) is record
      case Kind is
         when Bool_Kind =>
            Bool : Boolean;
         when Int_Kind =>
            Int : Int_Type;
         when Real_Kind =>
            Real : Real_Type;
         when Str_Kind =>
            Str  : WWUString;
      end case;
   end record;

   type Scalar is tagged record
      Data : Scalar_Data;
   end record;

   --  For the benefit of the child Operators

   package Base is
      --  Avoid primitiveness

      function New_Nil return Any;
      function New_Bool (Val : Boolean)   return Any;
      function New_Int  (Val : Int_Type)  return Any;
      function New_Real (Val : Real_Type) return Any;
      function New_Text (Val : Text)      return Any;
   end Base;

   --  Cursor type for iteration
   type Cursor_Kind is (Invalid, Map_Cursor, Vec_Cursor);

   type Cursor (Kind : Cursor_Kind := Invalid) is record
      case Kind is
         when Invalid =>
            null;
         when Map_Cursor =>
            Map_Pos : Any_Maps.Cursor;
         when Vec_Cursor =>
            Vec_Pos : Any_Vecs.Cursor;
      end case;
   end record;

end Yeison_Generic;
