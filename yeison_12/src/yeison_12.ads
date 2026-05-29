pragma Ada_2012;

with Ada.Iterator_Interfaces;

with Yeison_Generic.Operators;
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

   package Iteration is new
     Ada.Iterator_Interfaces (Impl.Cursor, Impl.Has_Element);

   type Any is new Impl.Any with null record with
     --  Constant_Indexing => Const_Ref,
     Variable_Indexing => Reference,
     --  Iteration via the standard Ada.Iterator_Interfaces machinery rather
     --  than the lightweight GNAT-specific Iterable aspect: the latter, on
     --  this controlled type, makes GNAT mis-generate deep finalization for
     --  types derived from Any that add controlled components (e.g.
     --  LML.Options.Pragmas.Input_Options), crashing at library finalization.
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Any;

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
   subtype Image_Formats is Impl.Image_Formats;

   subtype Scalar_Kinds is Impl.Scalar_Kinds;
   subtype Composite_Kinds is Impl.Composite_Kinds;
   use all type Kinds;

   subtype Text is Impl.Text;
   subtype UTF_8_String is String;

   function True return Any;
   function False return Any;

   ----------
   -- Maps --
   ----------

   function Keys (This : Any; Ordered : Boolean := False) return Any with
     Pre  => This.Kind = Map_Kind,
     Post => Keys'Result.Kind = Vec_Kind;

   -------------
   -- Vectors --
   -------------

   procedure Append (This : in out Any; Str : Text) with
     Pre => This.Kind = Vec_Kind;

   function Append (This : Any; Str : Text) return Any with
     Pre => This.Kind = Vec_Kind;

   ---------------
   -- Iterators --
   ---------------

   function First (Container : Any) return Impl.Cursor;

   function Next (Container : Any; Position : Impl.Cursor) return Impl.Cursor;

   function Has_Element (Container : Any; Position : Impl.Cursor) return Boolean;

   function Element (Container : Any; Position : Impl.Cursor) return Any;
   --  Also the Constant_Indexing function: yields the element at Position by
   --  value, which is what container-element iteration (for E of X) needs.

   function Iterate (Container : Any) return Iteration.Forward_Iterator'Class;
   --  Default_Iterator: lets clients write "for E of X loop ..." over the
   --  elements of a map (values) or vector.

   ----------------
   --  Indexing  --
   ----------------

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

   type Const (Element : not null access constant Any) is limited null record
     with Implicit_Dereference => Element;

   --  We need to recreate references for the access discriminant to use the
   --  proper type...

   --  The Constant_Indexing functions below cover read contexts. They are
   --  overloaded on the index so the same aspect serves both container-element
   --  iteration (for E of X, indexed by a cursor, returned by value) and key
   --  lookup in a read context (M (Key) / M ("str"), returned as a Const ref).

   function Constant_Reference (This     : Any;
                                Position : Impl.Cursor) return Any
                                renames Element;
   --  Cursor indexing for "for E of X": yields the element by value.

   function Constant_Reference (This : Any; Pos : Any) return Const with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;

   function Constant_Reference (This : Any; Pos : UTF_8_String) return Const
     with Pre => This.Kind = Map_Kind;
   --  See notes on Reference below; these are the read-only counterparts.

   function Reference (This : Any; Pos : Any) return Ref with
     Pre => Pos.Kind in Scalar_Kinds | Vec_Kind;
   --  Any may be a scalar, which will be used as key/index, or a vector that
   --  will be consumed one element at a time. In YAML, keys can be complex
   --  types, which is discouraged, and this is explicitly not supported.
   --
   --  If This is invalid, the appropriate holder value will be created (vec or
   --  map) depending on Any.Kind being Int or something else. If you want to
   --  force either one, assign first an empty value.

   function Reference (This : Any; Pos : UTF_8_String) return Ref with
     Pre => This.Kind in Map_Kind;
   --  Convenience to directly index with a string

   function Reference (This : Any; Pos : Big_Int) return Ref with
     Pre => This.Kind in Composite_Kinds;

   function Has_Key (This : Any; Key : UTF_8_String) return Boolean;
   function Has_Field (This : Any; Key : UTF_8_String) return Boolean
                       renames Has_Key;

   function Self (This : aliased Any) return Ref;

   ---------------
   -- Operators --
   ---------------

   package Operators is

      package Impl is new Yeison_12.Impl.Operators (Any);

      function "+" (This : Big_Int) return Any renames Impl.Make.Int;
      function "+" (This : Big_Real) return Any is
        (Impl.Make.Real (Reals.New_Real (This)));
      function "+" (This : Text) return Any renames Impl.Make.Str;
      function To_Vec (This : Impl.Any_Array) return Any renames Impl.Vec;

   end Operators;

   ----------
   -- Make --
   ----------

   package Make renames Operators.Impl.Make;

private

   Unimplemented : exception;

   function True return Any renames Make.True;
   function False return Any renames Make.False;

   --  Forward iterator used by Iterate/Default_Iterator. Holds a reference to
   --  the container and delegates cursor advancement to Impl's cursor logic.

   type Iterator is new Iteration.Forward_Iterator with record
      Container : access constant Any;
   end record;

   overriding function First (Object : Iterator) return Impl.Cursor;

   overriding function Next (Object   : Iterator;
                             Position : Impl.Cursor) return Impl.Cursor;

   -----------------
   -- Nicer_Image --
   -----------------

   function Nicer_Image (R : Big_Real) return Wide_Wide_String
   is (Yeison_Utils.Nicer_Real_Image (R'Wide_Wide_Image));

end Yeison_12;
