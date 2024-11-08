private with Ada.Numerics.Big_Numbers.Big_Integers;
private with Ada.Strings.Wide_Wide_Unbounded;

private with Daseot;

package Yeison_Daseot with Preelaborate is

   type Kinds is (Int_Kind,
                  Real_Kind,
                  Str_Kind,
                  Map_Kind,
                  Vec_Kind);

   subtype Scalar_Kinds is Kinds range Int_Kind .. Kinds'Pred (Map_Kind);

   subtype Text is Wide_Wide_String;

   type Any is tagged private with
     Aggregate => (Empty     => Empty_Map,
                   Add_Named => Initialize),
     Integer_Literal   => To_Int,
     String_Literal    => To_Str,
     Variable_Indexing => Reference;

   --  TODO: remove tagged once GNAT accepts dot notation for all private types

   type Ref (Element : not null access Any) is limited null record with
     Implicit_Dereference => Element;

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
     Aggregate => (Empty       => Empty_Vec,
                   Add_Unnamed => Initialize);

   package Make is

      --  We use this package to avoid primitiveness as we cannot use 'class
      --  around here.

      package Yeison renames Yeison_Daseot;

      function Vec (This : Yeison.Vec) return Any;

   end Make;

   function Empty_Vec return Vec;

   procedure Append (This : in out Vec;
                     Elem : Any'Class);

   procedure Initialize (This : in out Vec; Elem : Any'Class) renames Append;

private

   Unimplemented : exception;

   package Big_Ints renames Ada.Numerics.Big_Numbers.Big_Integers;

   subtype Big_Int is Big_Ints.Big_Integer;

   package WWUStrings renames Ada.Strings.Wide_Wide_Unbounded;
   subtype WWUString is WWUStrings.Unbounded_Wide_Wide_String;

   function "+" (S : Wide_Wide_String) return WWUString renames
     Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   type Scalars (Kind : Kinds) is record
      case Kind is
         when Int_Kind =>
            Int_Val : Big_Int;
         when Str_Kind =>
            Str_Val : WWUString;
         when Map_Kind | Vec_Kind =>
            null; -- Stored as dict/list in Daseot
         when others =>
            X : Integer := raise Unimplemented;
      end case;
   end record;

   package Trees is new Daseot (Scalars, Scalars'Image,
                                Text, "<", Text'Image);

   type Any is tagged record
      Tree : aliased Trees.Tree;
   end record;

   type Vec is new Any with null record;

   -------------
   --  Impls  --
   -------------

   ------------
   -- As_Int --
   ------------

   function As_Int (This : Any) return Integer
   is (This.Tree.Get.Int_Val.To_Integer);

   ---------------
   -- Empty_Map --
   ---------------

   function Empty_Map return Any
   is (Tree => Trees.Empty_Dict);

   ---------------
   -- Empty_Vec --
   ---------------

   function Empty_Vec return Vec
   is (Tree => Trees.Empty_List);

   -----------
   -- Image --
   -----------

   function Image (This : Any) return String is (This'Image);

   -------------
   -- Invalid --
   -------------

   function Invalid return Any is (others => <>);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Any) return Boolean
   is (This /= Invalid);

   ----------
   -- Kind --
   ----------

   function Kind (This : Any) return Kinds
   is (case This.Tree.Kind is
          when Trees.Atom_Kind =>
             This.Tree.Get.Kind,
          when Trees.Dict_Kind =>
             Map_Kind,
          when Trees.List_Kind =>
             Vec_Kind);

   ----------
   -- Self --
   ----------

   function Self (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Any
   is (Tree => Trees.Set
       ((Kind    => Int_Kind,
         Int_Val => Big_Ints.From_String (Img))));

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Any
   is (Tree => Trees.Set
       ((Kind    => Str_Kind,
         Str_Val => +Img)));

end Yeison_Daseot;
