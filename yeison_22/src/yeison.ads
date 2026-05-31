pragma Ada_2022;

with Ada.Finalization;
with Ada.Iterator_Interfaces;

private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Wide_Wide_Unbounded;

with Yeison_Utils;

package Yeison with Preelaborate is

   --  Self-contained Ada 2022 implementation. It deliberately does not reuse
   --  Yeison_12's storage: that type stores Yeison_12.Any in its containers, so
   --  a wrapper could not hand back zero-copy references of the user-facing
   --  type for "M (Key) := Value". Here the containers store this package's own
   --  Any, so indexing, references and "for E of X" are native. The only thing
   --  reused from the Yeison_12 crate is the type-agnostic Yeison_Utils.
   --
   --  Compared with Yeison_12 the user-facing surface is trimmed: the literal
   --  and aggregate aspects make the "+scalar" constructors and the extra
   --  Text/Int convenience overloads unnecessary (and, with the literal
   --  aspects, ambiguous), so they are gone. Construction is via literals,
   --  ["k" => v] map aggregates and +[...] vectors.

   subtype Big_Int  is Long_Long_Integer;
   subtype Big_Real is Long_Long_Float;

   subtype Universal_Integer is Long_Long_Integer;
   --  The widest integer we represent; doubles as vector index base.

   function Nicer_Image (R : Big_Real) return Wide_Wide_String;
   --  Avoid scientific notation when easy to do so

   package Reals is new Yeison_Utils.General_Reals (Big_Real, "<", Nicer_Image);

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
     Aggregate         => (Empty     => Empty_Map,
                           Add_Named => Insert),
     Integer_Literal   => To_Int,
     Real_Literal      => To_Real,
     String_Literal    => To_Str,
     Variable_Indexing => Reference,
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Any;
   --  The literal aspects let plain 1, 3.14 and "text" denote an Any; the
   --  Aggregate aspect enables ["key" => value] map literals; +[...] (see
   --  Yeison.Operators) builds vectors. Iteration uses Ada.Iterator_Interfaces
   --  rather than the GNAT-specific Iterable aspect (see Yeison_12 for why).

   function "=" (L, R : Any) return Boolean;
   --  Note: X = "text" / X = 1 work via the literal aspects on the right-hand
   --  side, so the Text/Int convenience overloads of Yeison_12 are not needed.

   function "<" (L, R : Any) return Boolean;
   function Precedes (L, R : Any) return Boolean renames "<";

   subtype Bool is Any with Dynamic_Predicate => Bool.Kind = Bool_Kind;
   subtype Int  is Any with Dynamic_Predicate => Int.Kind = Int_Kind;
   subtype Real is Any with Dynamic_Predicate => Real.Kind = Real_Kind;
   subtype Str  is Any with Dynamic_Predicate => Str.Kind = Str_Kind;
   subtype Vec  is Any with Dynamic_Predicate => Vec.Kind = Vec_Kind;
   --  Map cannot carry a Dynamic_Predicate: it is the container-Aggregate
   --  target, and GNAT default-initializes the aggregate's temporary as Map
   --  (Nil, before Empty/Add_Named run) and checks the predicate on it, which
   --  either fails (strict) or crashes finalization (Nil-tolerant). So Map is a
   --  plain documentation subtype; its Map_Kind is guaranteed by construction.
   subtype Map  is Any;

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
   function Is_True  (This : Any) return Boolean with Pre => This.Kind = Bool_Kind;
   function Is_False (This : Any) return Boolean with Pre => This.Kind = Bool_Kind;

   function Nil   return Any;
   function True  return Any;
   function False return Any;

   ---------------
   --  Scalars  --
   ---------------

   --  Literal aspect targets. They are public because the aspects reference
   --  them; clients use literals (1, 3.14, "text") rather than calling these.

   function To_Int  (Img : String) return Any;
   function To_Real (Img : String) return Any;
   function To_Str  (Img : Text)   return Any;

   --  Separate scalar type, to ease retrieval

   type Scalar (<>) is tagged private;

   function Kind (This : Scalar) return Scalar_Kinds;

   function As_Boolean (This : Scalar) return Boolean;
   function As_Integer (This : Scalar) return Big_Int;
   function As_Real    (This : Scalar) return Reals.General_Real;
   function As_Text    (This : Scalar) return Text;

   function Image (This   : Scalar;
                   Format : Image_Formats := Ada_Like) return Text;

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
   --  Raises Constraint_Error for non-finite values (Inf/NaN).

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

   procedure Insert (This  : in out Any;
                     Key   : Text;
                     Value : Any);
   --  Add_Named operation for ["key" => value] aggregates, and in-place
   --  string-keyed insertion. (A Text key, rather than Any, lets both string
   --  literals and Text variables be used as keys, and avoids a literal
   --  ambiguity with the functional Any-keyed Insert below. For in-place
   --  insertion with a non-string or replacing key use indexing: M (Key) := V.)

   function Insert (This    : Any;
                    Key     : Any;
                    Value   : Any;
                    Replace : Boolean := False)
                    return Any;
   --  Returns a *copy* with the new inserted value; keys may be any scalar.

   function Keys (This : Any; Ordered : Boolean := False) return Any with
     Pre  => This.Kind = Map_Kind,
     Post => Keys'Result.Kind = Vec_Kind;
   --  Keys, in either the original addition order, or in alphabetical order

   function Has_Key (This : Any; Key : Any) return Boolean with
     Pre => This.Kind = Map_Kind;

   ---------------
   --  Vectors  --
   ---------------

   function Empty_Vec return Any
     with Post => Empty_Vec'Result.Kind = Vec_Kind;

   procedure Append (This : in out Any; Elem : Any) with
     Pre => This.Kind = Vec_Kind;

   function Append (This : Any; Elem : Any) return Any with
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

   --  Indexing a mutable vector one past the end creates a new nil element
   --  there, allowing growth one element at a time. Vectors are 1-based. For
   --  constant vectors the index must be valid.

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   function Reference (This : in out Any; Pos : Any) return Ref with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Pos may be a scalar, used as key/index, or a vector consumed one element
   --  at a time (nested indexing). A nil This is auto-vivified into the proper
   --  holder (vec if Pos is Int, map otherwise).

   function Get (This : Any; Pos : Any) return Any with
     Pre => This.Kind in Composite_Kinds
            and then Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Always returns a copy; for in-place modification use Reference

   function Constant_Reference (This : Any; Pos : Any) return Const with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;

   ---------------
   -- Iterators --
   ---------------

   type Cursor (<>) is private;

   function Has_Element (Pos : Cursor) return Boolean;

   function First (This : Any) return Cursor;

   function Element (This : Any; Pos : Cursor) return Any;

   function Key (This : Any; Pos : Cursor) return Any;
   --  The map key at the cursor; raises Constraint_Error for a vector cursor.

   package Iteration is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (This : Any) return Iteration.Forward_Iterator'Class;

   function Constant_Reference (This : Any; Pos : Cursor) return Const;

   function Reference (This : in out Any; Pos : Cursor) return Ref;

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
   --  Implementation in the body, for a self-referential type and to control
   --  assignments via Controlled (when assigning through indexing).

   function Nil_Impl return Any_Impl_Ptr;

   type Any is new Ada.Finalization.Controlled with record
      Impl : Any_Impl_Ptr := Nil_Impl;
   end record with
     Type_Invariant => Impl /= null;

   overriding procedure Adjust (This : in out Any);

   overriding procedure Finalize (This : in out Any);

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

   type Iterator is new Iteration.Forward_Iterator with record
      Container : access constant Any;
   end record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next (Object   : Iterator;
                             Position : Cursor) return Cursor;

   -----------------
   -- Nicer_Image --
   -----------------

   function Nicer_Image (R : Big_Real) return Wide_Wide_String
   is (Yeison_Utils.Nicer_Real_Image (R'Wide_Wide_Image));

end Yeison;
