pragma Ada_2012;

with Ada.Finalization;
with Ada.Iterator_Interfaces;

private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Wide_Wide_Unbounded;

with Yeison_Utils;

package Yeison_12 with Preelaborate is

   --  Self-contained, generic-free implementation. (Historically this was a
   --  thin layer over a shared Yeison_Generic, reused between the Ada 2012 and
   --  Ada 2022 versions; that reuse caused more pain than it saved -- chiefly
   --  a parent/derived Any split -- so everything now lives here directly.)

   subtype Big_Int  is Long_Long_Integer;
   subtype Big_Real is Long_Long_Float;

   subtype Universal_Integer is Long_Long_Integer;
   --  The widest integer we represent; doubles as vector index base.

   package Reals renames Yeison_Utils.Big_Reals;
   --  Shared instance; Big_Real is Long_Long_Float, same as the other crate.

   subtype Text         is Wide_Wide_String;
   subtype UTF_8_String is String;

   -----------
   -- Kinds --
   -----------

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

   ---------
   -- Any --
   ---------

   type Any is new Ada.Finalization.Controlled with private with
     Variable_Indexing => Reference,
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Any;
   --  Iteration uses the standard Ada.Iterator_Interfaces machinery rather
   --  than the GNAT-specific Iterable aspect: the latter, on a controlled
   --  type, makes GNAT mis-generate deep finalization for types derived from
   --  Any that add controlled components (e.g. LML.Options.Pragmas.
   --  Input_Options), crashing at library finalization.

   overriding
   function "=" (L, R : Any) return Boolean;
   function "=" (L : Any;     R : Text)    return Boolean;
   function "=" (L : Text;    R : Any)     return Boolean;
   function "=" (L : Any;     R : Big_Int) return Boolean;
   function "=" (L : Big_Int;  R : Any)     return Boolean;
   function "=" (L : Any;      R : Big_Real) return Boolean;
   function "=" (L : Big_Real; R : Any)      return Boolean;

   function "<" (L, R : Any) return Boolean;
   function Precedes (L, R : Any) return Boolean renames "<";

   function "-" (Right : Any) return Any with
     Pre  => Right.Kind in Int_Kind | Real_Kind,
     Post => "-"'Result.Kind = Right.Kind;
   --  Unary negation of an integer or real value.

   subtype Bool is Any with Dynamic_Predicate => Bool.Kind = Bool_Kind;
   subtype Int  is Any with Dynamic_Predicate => Int.Kind = Int_Kind;
   subtype Real is Any with Dynamic_Predicate => Real.Kind = Real_Kind;
   subtype Str  is Any with Dynamic_Predicate => Str.Kind = Str_Kind;
   subtype Map  is Any with Dynamic_Predicate => Map.Kind = Map_Kind;
   subtype Vec  is Any with Dynamic_Predicate => Vec.Kind = Vec_Kind;

   type Any_Array is array (Positive range <>) of Any;

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

   function Is_Nil   (This : Any) return Boolean is (This.Kind = Nil_Kind);
   function Is_True  (This : Any) return Boolean
     with Pre => This.Kind = Bool_Kind;
   --  Raises Assertion_Error (or Constraint_Error in contracts-off mode) if
   --  This is not a Bool_Kind value.
   function Is_False (This : Any) return Boolean
     with Pre => This.Kind = Bool_Kind;

   ---------------
   --  Scalars  --
   ---------------

   --  Separate type to ease initializations elsewhere

   type Scalar (<>) is tagged private;

   function Kind (This : Scalar) return Scalar_Kinds;

   function As_Boolean (This : Scalar) return Boolean;
   function As_Integer (This : Scalar) return Big_Int;
   function As_Real    (This : Scalar) return Reals.General_Real;
   function As_Text    (This : Scalar) return Text;

   function Image (This   : Scalar;
                   Format : Image_Formats := Ada_Like) return Text;

   package Scalars is
      function New_Bool (Val : Boolean)            return Scalar;
      function New_Int  (Val : Big_Int)            return Scalar;
      function New_Real (Val : Reals.General_Real) return Scalar;
      function New_Text (Val : Text)               return Scalar;
   end Scalars;

   --  Retrieval

   function As_Scalar (This : Any'Class) return Scalar
     with Pre => This.Kind in Scalar_Kinds;

   function As_Bool (This : Any) return Boolean
     with Pre => This.Kind = Bool_Kind;

   function As_Int (This : Any) return Big_Int
     with Pre => This.Kind = Int_Kind;

   function As_Real (This : Any) return Reals.General_Real
     with Pre => This.Kind = Real_Kind;

   function As_Real_Float (This : Any) return Big_Real
     with Pre => This.Kind = Real_Kind;
   --  Convenience: directly extract the underlying Big_Real without going
   --  through Reals.General_Real.  Raises Constraint_Error for non-finite
   --  values (Inf/NaN) that have no Big_Real representation.

   function As_Text (This : Any) return Text
     with Pre => This.Kind = Str_Kind;

   function As_UTF_8 (This : Any) return String
     with Pre => This.Kind = Str_Kind;

   function As_Latin_1 (This : Any) return String
     with Pre => This.Kind = Str_Kind;

   --  Overloaded renamings
   function Get (This : Any'Class) return Scalar            renames As_Scalar;
   function Get (This : Any) return Boolean                 renames As_Bool;
   function Get (This : Any) return Big_Int                 renames As_Int;
   function Get (This : Any) return Reals.General_Real      renames As_Real;
   function Get (This : Any) return Text                    renames As_Text;

   -------------------
   --  Collections  --
   -------------------

   function Is_Empty (This : Any) return Boolean with
     Pre => This.Is_Nil or else This.Kind in Composite_Kinds;

   function Length (This : Any) return Universal_Integer with
     Pre => This.Kind in Composite_Kinds;

   ------------
   --  Maps  --
   ------------

   function Empty_Map return Any
     with Post => Empty_Map'Result.Kind = Map_Kind;

   procedure Insert (This    : in out Any;
                     Key     : Any;
                     Value   : Any;
                     Replace : Boolean := False);

   function Insert (This    : Any;
                    Key     : Any;
                    Value   : Any;
                    Replace : Boolean := False)
                    return Any;
   --  Returns a *copy* with the new inserted value

   function Keys (This : Any; Ordered : Boolean := False) return Any with
     Pre  => This.Kind = Map_Kind,
     Post => Keys'Result.Kind = Vec_Kind;
   --  Keys, in either the original addition order, or in alphabetical order

   function Has_Key (This : Any; Key : Any) return Boolean with
     Pre => This.Kind = Map_Kind;

   function Has_Key (This : Any; Key : UTF_8_String) return Boolean;
   function Has_Field (This : Any; Key : UTF_8_String) return Boolean
                       renames Has_Key;

   ---------------
   --  Vectors  --
   ---------------

   function Empty_Vec return Any
     with Post => Empty_Vec'Result.Kind = Vec_Kind;

   procedure Append (This : in out Any; Elem : Any) with
     Pre => This.Kind = Vec_Kind;

   function Append (This : Any; Elem : Any) return Any with
     Pre => This.Kind = Vec_Kind;

   procedure Append (This : in out Any; Str : Text) with
     Pre => This.Kind = Vec_Kind;

   function Append (This : Any; Str : Text) return Any with
     Pre => This.Kind = Vec_Kind;

   function First_Index (This : Any) return Universal_Integer with
     Pre => This.Kind = Vec_Kind;

   function Last_Index (This : Any) return Universal_Integer with
     Pre => This.Kind = Vec_Kind;

   function Head (This : Any) return Any with
     Pre => This.Kind = Vec_Kind and then not This.Is_Empty;

   function Tail (This : Any) return Any with
     Pre  => This.Kind = Vec_Kind and then not This.Is_Empty,
     Post => Tail'Result.Length = This.Length - 1;

   ----------------
   --  Indexing  --
   ----------------

   --  For the special case of indexing a mutable vector, we allow indexing one
   --  past then end, and that will create a new nil element at that position.
   --  This Allows growing a vector one element at a time. Vectors are 1-based.

   --  For constant vectors, the index must be valid.

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   function Reference (This : in out Any; Pos : Any) return Ref with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Pos may be a scalar, used as key/index, or a vector consumed one element
   --  at a time (nested indexing). If This is nil, the appropriate holder (vec
   --  or map) is created depending on Pos.Kind being Int or something else.

   function Reference (This : in out Any; Pos : UTF_8_String) return Ref with
     Pre => This.Is_Nil or else This.Kind = Map_Kind;
   --  A nil This is auto-vivified into a map (enables nested building).

   function Reference (This : in out Any; Pos : Big_Int) return Ref with
     Pre => This.Is_Nil or else This.Kind in Composite_Kinds;
   --  A nil This is auto-vivified into a vector (enables nested building).

   function Get (This : Any; Pos : Any) return Any with
     Pre => This.Kind in Composite_Kinds
            and then Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Always returns a copy; for in-place modification use Reference

   --  Constant indexing. All overloads return a read-only view (Const) of the
   --  designated element rather than a copy, so that "for E of X" and explicit
   --  constant indexing do not materialize premature copies. A uniform return
   --  type is required for the cursor overload, which drives "for E of X".

   function Constant_Reference (This : Any; Pos : Any) return Const with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;

   function Constant_Reference (This : Any; Pos : UTF_8_String) return Const
     with Pre => This.Kind = Map_Kind;

   function Constant_Reference (This : Any; Pos : Big_Int) return Const with
     Pre => This.Kind in Composite_Kinds;

   ---------------
   -- Iterators --
   ---------------

   type Cursor (<>) is private;

   function Has_Element (Pos : Cursor) return Boolean;
   --  Cursor-only validity test (the formal for Ada.Iterator_Interfaces).

   function First (This : Any) return Cursor;
   --  Cursor to the first element; Invalid if empty.

   function Element (This : Any; Pos : Cursor) return Any;

   function Key (This : Any; Pos : Cursor) return Any;
   --  The map key at the given cursor position; raises Constraint_Error if Pos
   --  is not a map cursor. This mirrors Element, which returns the value.

   package Iteration is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (This : Any) return Iteration.Forward_Iterator'Class;
   --  Default_Iterator: enables "for E of X loop ..." over a map (values) or
   --  vector. Constant_Indexing below supplies the element.

   function Constant_Reference (This : Any; Pos : Cursor) return Const;
   --  Cursor indexing for "for E of X" over a constant container.

   function Reference (This : in out Any; Pos : Cursor) return Ref;
   --  Cursor indexing for "for E of X" over a variable container (yields a
   --  modifiable view of the element).

   ----------
   -- Make --
   ----------

   package Make is
      function Nil return Any;

      function True  return Any;
      function False return Any;

      function Bool (This : Boolean)            return Any;
      function Int  (This : Big_Int)            return Any;
      function Real (This : Reals.General_Real) return Any;
      function Real (This : Big_Real)           return Any;
      --  Convenience overload: wraps Big_Real in a finite General_Real
      function Str  (This : Text)               return Any;

      function Scalar (This : Yeison_12.Scalar) return Any;

      function Map return Any;
      function Vec return Any;
   end Make;

   function True  return Any renames Make.True;
   function False return Any renames Make.False;

   ---------------
   -- Operators --
   ---------------

   package Operators is

      function "+" (This : Boolean)  return Any renames Make.Bool;
      function "+" (This : Big_Int)  return Any renames Make.Int;
      function "+" (This : Big_Real) return Any renames Make.Real;
      function "+" (This : Text)     return Any renames Make.Str;

      function "/" (L, R : Any) return Any with
        Pre  => L.Kind in Scalar_Kinds | Vec_Kind,
        Post => "/"'Result.Kind = Vec_Kind;
      --  Append/build a vector

      function To_Vec (This : Any_Array) return Any with
        Post => To_Vec'Result.Kind = Vec_Kind;

   end Operators;

private

   Unimplemented : exception;

   package WWUStrings renames Ada.Strings.Wide_Wide_Unbounded;
   subtype WWUString is WWUStrings.Unbounded_Wide_Wide_String;

   function U (S : Wide_Wide_String) return WWUString renames
     Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   function S (U : WWUString) return Text renames
     Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   type Any_Impl;

   type Any_Impl_Ptr is access Any_Impl;
   --  Implementation is in the body, both for a self-referential type and to
   --  control assignments via Controlled (when assigning through indexing).

   function Nil_Impl return Any_Impl_Ptr;

   type Any is new Ada.Finalization.Controlled with record
      Impl : Any_Impl_Ptr := Nil_Impl;
   end record with
     Type_Invariant => Impl /= null;

   overriding procedure Adjust (This : in out Any);

   overriding procedure Finalize (This : in out Any);

   --  Containers store Any directly (a definite, owning, controlled type); no
   --  class-wide storage is needed now that there is a single Any type.

   package Any_Maps is new Ada.Containers.Ordered_Maps (Any, Any);

   subtype Universal_Positive is
     Universal_Integer range 1 .. Universal_Integer'Last;

   package Any_Vecs is new Ada.Containers.Vectors (Universal_Positive, Any);

   ---------------
   --  Scalars  --
   ---------------

   type Scalar_Data (Kind : Scalar_Kinds := Bool_Kind) is record
      case Kind is
         when Bool_Kind => Bool : Boolean;
         when Int_Kind  => Int  : Big_Int;
         when Real_Kind => Real : Reals.General_Real;
         when Str_Kind  => Str  : WWUString;
      end case;
   end record;

   type Scalar is tagged record
      Data : Scalar_Data;
   end record;

   --  Cursor type for iteration

   type Cursor_Kind is (Invalid, Map_Cursor, Vec_Cursor);

   type Cursor (Kind : Cursor_Kind := Invalid) is record
      case Kind is
         when Invalid    => null;
         when Map_Cursor => Map_Pos : Any_Maps.Cursor;
         when Vec_Cursor => Vec_Pos : Any_Vecs.Cursor;
      end case;
   end record;

   --  Forward iterator used by Iterate/Default_Iterator. Holds a reference to
   --  the container and delegates cursor advancement to the cursor logic.

   type Iterator is new Iteration.Forward_Iterator with record
      Container : access constant Any;
   end record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next (Object   : Iterator;
                             Position : Cursor) return Cursor;

end Yeison_12;
