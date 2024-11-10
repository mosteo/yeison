with Ada.Finalization;

private with Ada.Numerics.Big_Numbers.Big_Integers;
private with Ada.Numerics.Big_Numbers.Big_Reals;
private with Ada.Strings.Wide_Wide_Unbounded;

package Yeison with Preelaborate is

   type Kinds is (Bool_Kind,
                  Int_Kind,
                  Real_Kind,
                  Str_Kind,
                  Map_Kind,
                  Vec_Kind);

   subtype Scalar_Kinds is Kinds range Kinds'First .. Kinds'Pred (Map_Kind);

   subtype Text is Wide_Wide_String;

   type Any is new Ada.Finalization.Controlled with private with
     Aggregate => (Empty     => Empty_Map,
                   Add_Named => Initialize),
     Integer_Literal   => To_Int,
     String_Literal    => To_Str,
     Constant_Indexing => Const_Ref,
     Variable_Indexing => Reference;

   --  TODO: remove tagged once GNAT accepts dot notation for all private types

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   --------------
   --  Common  --
   --------------

   function Image (This : Any) return String;
   --  UTF-8-encoded

   function Invalid return Any;
   --  An uninitialized Any; using it as the RHS of assignments will fail

   function Is_Valid (This : Any) return Boolean;

   function Kind (This : Any) return Kinds with
     Pre => This.Is_Valid;

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
   --  A reference without indexing, mainly useful for testing

   ---------------
   --  Scalars  --
   ---------------

   function True return Any;

   function False return Any;

   function As_Bool return Boolean;

   function As_Int (This : Any) return Integer
     with Pre => This.Kind = Int_Kind;

   function To_Int (Img : String) return Any;

   function To_Str (Img : Wide_Wide_String) return Any;

   subtype Str is Any with
     Dynamic_Predicate => Str.Kind = Str_Kind;

   ------------
   --  Maps  --
   ------------

   function Empty_Map return Any
     with Post => Empty_Map'Result.Kind = Map_Kind;

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

   ---------------
   --  Vectors  --
   ---------------

   procedure Append (This : in out Any; Elem : Any) with
     Pre => This.Kind = Vec_Kind;

   --  This type is a clutch until GNAT accepts both map/vector initializers

   type Vec is private with
     Aggregate => (Empty       => Empty_Any_Vec,
                   Add_Unnamed => Initialize);

   function Empty_Vec return Any;

   package Make is

      --  We use this package to avoid primitiveness as we cannot use 'class
      --  around here.

      function Vec (This : Yeison.Vec) return Any;

   end Make;

   procedure Append (This : in out Vec;
                     Elem : Any'Class);

   function Empty_Any_Vec return Vec;

   procedure Initialize (This : in out Vec; Elem : Any'Class) renames Append;

private

   Unimplemented : exception;

   package Big_Ints renames Ada.Numerics.Big_Numbers.Big_Integers;
   subtype Big_Int is Big_Ints.Big_Integer;

   package Big_Reals renames Ada.Numerics.Big_Numbers.Big_Reals;
   subtype Big_Real is Big_Reals.Big_Real;

   package WWUStrings renames Ada.Strings.Wide_Wide_Unbounded;
   subtype WWUString is WWUStrings.Unbounded_Wide_Wide_String;

   function "+" (S : Wide_Wide_String) return WWUString renames
     Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   type Any_Impl;

   type Any_Impl_Ptr is access Any_Impl;

   type Any is new Ada.Finalization.Controlled with record
      Impl : Any_Impl_Ptr;
   end record;

   function "<" (L, R : Any) return Boolean;

   overriding procedure Adjust (This : in out Any);

   overriding procedure Finalize (This : in out Any);

   type Vec is new Any with null record;

end Yeison;
