pragma Ada_2012;

with Ada.Finalization;

--  private with Ada.Numerics.Big_Numbers.Big_Integers;
--  private with Ada.Numerics.Big_Numbers.Big_Reals;
private with Ada.Strings.Wide_Wide_Unbounded;

generic
   --  Because big numbers aren't available in Ada 2012, and no other
   --  convenient implementation is available that I know, we just
   --  delegate this impl to clients.
   type Int_Type is private; -- not range <> to allow Big_Integer
   with function To_Integer (I : Int_Type) return Long_Long_Integer;
   with function Image (I : Int_Type) return Wide_Wide_String is <>;
   type Real_Type is private; -- same about digits <>
   with function Image (R : Real_Type) return Wide_Wide_String is <>;
   with function "<" (L, R : Int_Type) return Boolean is <>;
   with function "<" (L, R : Real_Type) return Boolean is <>;
package Yeison_Generic is

   --  This should be Preelaborate but a suspicious errors in the body
   --  precludes it for now. TODO: To be investigated.

   type Kinds is (Bool_Kind,
                  Int_Kind,
                  Real_Kind,
                  Str_Kind,
                  Map_Kind,
                  Vec_Kind);

   subtype Scalar_Kinds is Kinds range Kinds'First .. Kinds'Pred (Map_Kind);

   subtype Composite_Kinds is Kinds range Map_Kind .. Kinds'Last;

   subtype Text is Wide_Wide_String;

   type Any is new Ada.Finalization.Controlled with private;
   --  No aspects here. This forces us to expose them in Yeison_12 and
   --  Yesion_22, which involves some duplication, but otherwise there are
   --  some ambiguities that rain on our parade...

   --  Explicit types

   subtype Any_Bool is Any with Dynamic_Predicate => Any_Bool.Kind = Bool_Kind;

   subtype Any_Str is Any with Dynamic_Predicate => Any_Str.Kind = Str_Kind;

   subtype Any_Scalar is Any with
     Dynamic_Predicate => Any_Scalar.Kind in Scalar_Kinds;

   subtype Any_Composite is Any with
     Dynamic_Predicate => Any_Composite.Kind in Composite_Kinds;

   subtype Any_Map is Any with Dynamic_Predicate => Any_Map.Kind = Map_Kind;

   subtype Any_Vec is Any with Dynamic_Predicate => Any_Vec.Kind = Vec_Kind;

   --  TODO: remove tagged once GNAT accepts dot notation for all private types

   --  type Ref (Element : not null access Any) is limited null record with
   --    Implicit_Dereference => Element;
   --
--  type Const (Element : not null access constant Any) is limited null record
   --    with Implicit_Dereference => Element;

   --------------
   --  Common  --
   --------------

   type Image_Formats is (Ada_Like, JSON);

   function Image (This    : Any'Class;
                   Format  : Image_Formats := Ada_Like;
                   Compact : Boolean := False)
                   return Text;

   function Invalid return Any;
   --  An uninitialized Any; using it as the RHS of assignments will fail

   function Is_Valid (This : Any) return Boolean;

   function Kind (This : Any) return Kinds with
     Pre => This.Is_Valid;

   --  function Const_Ref (This : aliased Any; Pos : Any) return Const with
   --    Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  --  See notes on Reference below. Same applies, except for the
   --  --  initialization of empty maps/vectors.
   --
   --  function Reference (This : aliased Any; Pos : Any) return Ref with
   --    Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Any may be a scalar, which will be used as key/index, or a vector that
   --  will be consumed one element at a time. In YAML, keys can be complex
   --  types, which is discouraged, and this is explicitly not supported.
   --
   --  If This is invalid, the appropriate holder value will be created (vec or
   --  map) depending on Any.Kind being Int or something else. If you want to
   --  force either one, assign first an empty value.

--   function Self (This : aliased Any) return Ref;
   --  A reference without indexing, mainly useful for testing

   ---------------
   --  Scalars  --
   ---------------

   function True return Any;

   function False return Any;

   function As_Bool (This : Any) return Boolean;

   function As_Int (This : Any) return Long_Long_Integer;

   function Make_Int (This : Int_Type) return Any;

   function Make_Str (This : Text) return Any;

   ------------
   --  Maps  --
   ------------

   function Empty_Map return Any
     with Post => Empty_Map'Result in Any_Map;

   --  Not named Insert to avoid ambiguities with Add_Named aspect
   procedure Initialize (This  : in out Any;
                         Key   : Text;
                         Value : Any);
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

   ---------------
   --  Vectors  --
   ---------------

   procedure Append (This : in out Any; Elem : Any) with
     Pre => This in Any_Vec;

   function Empty_Vec return Any;

   ----------------
   --  Indexing  --
   ----------------

   function Get (This : Any; Pos : Any) return Any with
     Pre => This in Any_Composite and then Pos in Any_Scalar;

   package Operators is

      function "/" (L, R : Any) return Any with
        Pre  => L.Kind in Scalar_Kinds | Vec_Kind,
        Post => "/"'Result in Any_Vec;

   end Operators;

   -------------------
   --  Boilerplate  --
   -------------------

   function To_Str (Img : Wide_Wide_String) return Any;

private

   Unimplemented : exception;

   --  package Big_Ints renames Ada.Numerics.Big_Numbers.Big_Integers;
   --  subtype Big_Int is Big_Ints.Big_Integer;
   --
   --  package Big_Reals renames Ada.Numerics.Big_Numbers.Big_Reals;
   --  subtype Big_Real is Big_Reals.Big_Real;

   package WWUStrings renames Ada.Strings.Wide_Wide_Unbounded;
   subtype WWUString is WWUStrings.Unbounded_Wide_Wide_String;

   function "+" (S : Wide_Wide_String) return WWUString renames
     Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   type Any_Impl;

   type Any_Impl_Ptr is access Any_Impl;
   --  Implementation is in body, so we can have a self-referential type but
   --  also to control assignments via Controlled (when assigning through
   --  indexing).

   type Any is new Ada.Finalization.Controlled with record
      Impl : Any_Impl_Ptr;
   end record;

   function "<" (L, R : Any) return Boolean;

   overriding procedure Adjust (This : in out Any);

   overriding procedure Finalize (This : in out Any);

   type Vec is new Any with null record;

end Yeison_Generic;
