with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off);
with GNAT.IO; use GNAT.IO;
pragma Warnings (On);

with Yeison_Utils;

package body Yeison_Generic is

   package Fixed renames Ada.Strings.Wide_Wide_Fixed;

   use type Ada.Containers.Count_Type;
   use all type Ada.Strings.Trim_End;

   pragma Warnings (Off);
   function Encode (T : Text; Output_BOM : Boolean  := False) return String
                    renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode;

   function Decode (T : String) return Text
                    renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode;
   pragma Warnings (On);

   subtype Any_Parent is Ada.Finalization.Controlled;

   type Any_Impl (Kind : Kinds := Bool_Kind) is record
      case Kind is
         when Nil_Kind => null;

         when Scalar_Kinds =>
            Val : Scalar_Data (Kind);

         when Map_Kind =>
            Map  : Any_Maps.Map;
            Keys : Any_Vecs.Vector;
            --  Keys, in the order in which they were added

         when Vec_Kind =>
            Vec : Any_Vecs.Vector;
      end case;
   end record
     with Dynamic_Predicate =>
       (if Any_Impl.Kind = Map_Kind then Map.Length = Keys.Length);

   --------------
   -- Nil_Impl --
   --------------

   function Nil_Impl return Any_Impl_Ptr
   is (new Any_Impl'(Kind => Nil_Kind));

   function Kind (This : Scalar) return Scalar_Kinds is (This.Data.Kind);

   function As_Boolean (This : Scalar) return Boolean is (This.Data.Bool);
   function As_Integer (This : Scalar) return Int_Type is (This.Data.Int);
   function As_Real (This : Scalar) return Real_Type is (This.Data.Real);
   function As_Text (This : Scalar) return Text
   is (WWUStrings.To_Wide_Wide_String (This.Data.Str));

   ----------
   -- Base --
   ----------

   package body Base is

      function New_Nil return Any
      is (Any'(Any_Parent with Impl => Nil_Impl));

      function New_Bool (Val : Boolean)   return Any
      is (Any'(Any_Parent with Impl =>
                  new Any_Impl'(Bool_Kind, (Bool_Kind, Val))));

      function New_Int  (Val : Int_Type)  return Any
      is (Any'(Any_Parent with Impl =>
                  new Any_Impl'(Int_Kind, (Int_Kind, Val))));

      function New_Real (Val : Real_Type) return Any
      is (Any'(Any_Parent with Impl =>
                  new Any_Impl'(Real_Kind, (Real_Kind, Val))));

      function New_Text (Val : Text)      return Any
      is (Any'(Any_Parent with Impl =>
                  new Any_Impl'(Str_Kind, (Str_Kind, U (Val)))));

   end Base;

   -------------
   -- Scalars --
   -------------

   package body Scalars is

      -----------------
      -- New_Boolean --
      -----------------

      function New_Bool (Val : Boolean) return Scalar
      is (Data => (Kind => Bool_Kind,
                   Bool => Val));

      -------------
      -- New_Int --
      -------------

      function New_Int     (Val : Int_Type)  return Scalar
      is (Data => (Kind => Int_Kind,
                   Int  => Val));

      --------------
      -- New_Real --
      --------------

      function New_Real    (Val : Real_Type) return Scalar
      is (Data => (Kind => Real_Kind,
                   Real => Val));

      --------------
      -- New_Text --
      --------------

      function New_Text (Val : Text) return Scalar
      is (Data => (Kind => Str_Kind,
                   Str  => U (Val)));

   end Scalars;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Any_Impl) return Boolean is
      use type WWUString;
   begin
      if L.Kind < R.Kind then
         return True;
      elsif R.Kind < L.Kind then
         return False;
      end if;

      --  Both the same

      case L.Kind is
         when Nil_Kind => return False;
         when Bool_Kind => return L.Val.Bool < R.Val.Bool;
         when Int_Kind  => return L.Val.Int < R.Val.Int;
         when Real_Kind => return L.Val.Real < R.Val.Real;
         when Str_Kind  => return L.Val.Str < R.Val.Str;
         when Map_Kind  => raise Unimplemented;
         when Vec_Kind  => raise Unimplemented;
      end case;
   end "<";

   ---------------
   -- As_Scalar --
   ---------------

   function As_Scalar (This : Any'Class) return Scalar
   is (Scalar'(Data => This.Impl.Val));

   -------------
   -- As_Bool --
   -------------

   function As_Bool (This : Any) return Boolean
   is (This.Impl.Val.Bool);

   ------------
   -- As_Int --
   ------------

   function As_Int (This : Any) return Int_Type
   is (This.Impl.Val.Int);

   -------------
   -- As_Real --
   -------------

   function As_Real (This : Any) return Real_Type
   is (This.Impl.Val.Real);

   -------------
   -- As_Text --
   -------------

   function As_Text (This : Any) return Text
   is (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
       (This.Impl.Val.Str));

   ---------------
   -- Empty_Map --
   ---------------

   function Empty_Map return Any
   is (Ada.Finalization.Controlled with
         Impl => new Any_Impl'(Kind => Map_Kind, others => <>));

   ---------------
   -- Empty_Vec --
   ---------------

   function Empty_Vec return Any
   is (Ada.Finalization.Controlled with
         Impl => new Any_Impl'(Kind => Vec_Kind, others => <>));

   ----------------
   -- JSON_Quote --
   ----------------

   function JSON_Quote (Str : Text) return Text is
   begin
      return '"' & Yeison_Utils.JSON_Escape (Str) & '"';
   end JSON_Quote;

   -----------
   -- Image --
   -----------

   function Image (This    : Any'Class;
                   Format  : Image_Formats := Ada_Like;
                   Options : Image_Options := (others => <>))
                   return Text
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      function "+" (S : Wide_Wide_String) return WWUString
                    renames To_Unbounded_Wide_Wide_String;
      pragma Unreferenced ("+");

      ------------------
      -- Scalar_Image --
      ------------------

      function Scalar_Image (This : Any'Class) return Text
      is (case This.Kind is
             when Bool_Kind       =>
                (if This.Impl.Val.Bool then "true" else "false"),
             when Int_Kind        =>
                Fixed.Trim (Image (This.Impl.Val.Int), Side => Both),
             when Real_Kind       =>
                Fixed.Trim (Image (This.Impl.Val.Real), Side => Both),
             when Str_Kind        =>
               (case Format is
                   when Ada_Like =>
                      To_Wide_Wide_String (This.Impl.Val.Str),
                   when JSON     =>
                      JSON_Quote (To_Wide_Wide_String (This.Impl.Val.Str))),
             when Nonscalar_Kinds =>
                raise Program_Error with "not a scalar: " & This.Kind'Image
         );

      Result : WWUString;

      ---------------------
      -- Empty_Map_Image --
      ---------------------

      function Empty_Map_Image return Text
      is (case Format is
             when Ada_Like => "[=>]",
             when JSON     => "{}");

      --------------
      -- Map_Open --
      --------------

      function Map_Open return Text
      is (case Format is
             when Ada_Like => "[",
             when JSON     => "{");

      ---------------
      -- Map_Close --
      ---------------

      function Map_Close return Text
      is (case Format is
             when Ada_Like => "]",
             when JSON     => "}");

      ---------------
      -- Map_Arrow --
      ---------------

      function Map_Arrow return Text
      is (case Format is
             when Ada_Like => " => ",
             when JSON     => ": ");

      ---------------------
      -- Empty_Vec_Image --
      ---------------------

      function Empty_Vec_Image return Text
      is (case Format is
             when Ada_Like => "[,]",
             when JSON     => "[]");

      --------------
      -- Vec_Open --
      --------------

      function Vec_Open return Text
      is (case Format is
             when Ada_Like => "[",
             when JSON     => "[");

      ---------------
      -- Vec_Close --
      ---------------

      function Vec_Close return Text
      is (case Format is
             when Ada_Like => "]",
             when JSON     => "]");

      --------------
      -- Traverse --
      --------------

      procedure Traverse (This   : Any'Class;
                          Prefix : Text;
                          Contd  : Boolean := False)
      is
         NL  : constant Text := "" & Ada.Characters.Wide_Wide_Latin_1.LF;
         Tab : constant Text :=
                 (case Format is
                     when Ada_Like => "   ",
                     when JSON     => "  ");
         --  function WS (Str : Text) return Text -- Whitespace of same length
         --  is (1 .. Str'Length => ' ');
      begin
         case This.Kind is
            when Nil_Kind =>
               Append (Result, (if Contd then Text'("") else Prefix) & "null");

            when Scalar_Kinds =>
               Append (Result,
                       (if Contd then Text'("") else Prefix)
                       & Scalar_Image (This));

            when Map_Kind =>
               declare
                  C_Map : Any_Maps.Cursor := This.Impl.Map.First;
                  C_Vec : Any_Vecs.Cursor := This.Impl.Keys.First;
                  use Any_Maps;
                  use Any_Vecs;
                  Abbr : constant Boolean :=
                           Options.Compact and then This.Impl.Map.Length in 1;
               begin
                  if This.Impl.Map.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & Empty_Map_Image);
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & Map_Open
                          & (if Abbr then " " else NL));

                  while (if Options.Ordered_Keys
                         then Any_Maps.Has_Element (C_Map)
                         else Any_Vecs.Has_Element (C_Vec))
                  loop
                     Append (Result,
                             (if Abbr then " " else Prefix & Tab)
                             & (if Options.Ordered_Keys
                                then Any_Maps.Key (C_Map)
                                             .Image (Format, Options)
                                else Any_Vecs.Element (C_Vec)
                                             .Image (Format, Options))
                             & Map_Arrow);
                     --  TODO: the above key image should be prefixed in case
                     --  we are using an object for indexing.

                     Traverse ((if Options.Ordered_Keys
                               then This.Impl.Map.Constant_Reference (C_Map)
                               else This.Impl.Map.Constant_Reference
                                 (This.Impl.Map.Find
                                    (Any_Vecs.Element (C_Vec)))),
                               Prefix & Tab,
                               Contd => True);

                     if (if Options.Ordered_Keys
                         then Any_Maps.Has_Element (Next (C_Map))
                         else Any_Vecs.Has_Element (Next (C_Vec)))
                     then
                        Append (Result, ",");
                     end if;

                     if not Abbr then
                        Append (Result, NL);
                     end if;

                     if Options.Ordered_Keys then
                        Next (C_Map);
                     else
                        Next (C_Vec);
                     end if;
                  end loop;

                  Append (Result,
                          (if Abbr then " " else Prefix) & Map_Close);
               end;

            when Vec_Kind =>
               declare
                  Abbr : constant Boolean :=
                           Options.Compact and then This.Impl.Vec.Length in 1;
                  I    : Natural := 0;
               begin
                  if This.Impl.Vec.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix)
                              & Empty_Vec_Image);
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & Vec_Open
                          & (if Abbr then " " else NL));

                  for E of This.Impl.Vec loop
                     Traverse (E,
                               Prefix & Tab,
                               Contd => Abbr);
                     I := I + 1;
                     Append (Result,
                             (if I = Natural (This.Impl.Vec.Length)
                              then ""
                              else ",")
                             & (if Abbr then " " else NL));
                  end loop;
                  Append (Result,
                          (if Abbr then "" else Prefix)
                          & Vec_Close);
               end;
         end case;
      end Traverse;

   begin
      Traverse (This, "");

      return To_Wide_Wide_String (Result);
   end Image;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Any) return Boolean
   is (case This.Kind is
          when Map_Kind => This.Impl.Map.Is_Empty,
          when Vec_Kind => This.Impl.Vec.Is_Empty,
          when others   =>
             raise Constraint_Error
               with "not a collection: " & This.Kind'Image);

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (This : Any) return Boolean
   is (This.Kind /= Nil_Kind);

   ----------
   -- Keys --
   ----------

   function Keys (This : Any; Ordered : Boolean := False) return Any_Array is
      Result : Any_Array (1 .. Integer (This.Impl.Map.Length));
      Pos    : Positive := 1;
   begin
      if Ordered then
         for I in This.Impl.Map.Iterate loop
            Result (Pos) := Any (Any_Maps.Key (I));
            Pos := Pos + 1;
         end loop;
      else
         for Key of This.Impl.Keys loop
            Result (Pos) := Any (Key);
            Pos := Pos + 1;
         end loop;
      end if;

      return Result;
   end Keys;

   ----------
   -- Kind --
   ----------

   function Kind (This : Any) return Kinds
   is (This.Impl.Kind);

   ------------
   -- Length --
   ------------

   function Length (This : Any) return Universal_Integer
   is (case This.Kind is
          when Map_Kind => Universal_Integer (This.Impl.Map.Length),
          when Vec_Kind => Universal_Integer (This.Impl.Vec.Length),
          when others   =>
             raise Constraint_Error
               with "not a collection: " & This.Kind'Image);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Any) return Boolean is
   begin
      if L.Kind < R.Kind then
         return True;
      elsif R.Kind < L.Kind then
         return False;
      end if;

      return L.Impl.all < R.Impl.all;
   end "<";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (This : in out Any) is
   begin
      This.Impl := new Any_Impl'(This.Impl.all);
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Any; Elem : Any) is
   begin
      This.Impl.Vec.Append (Elem);
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert (This    : in out Any;
                     Key     : Any;
                     Value   : Any;
                     Replace : Boolean := False)
   is
   begin
      This.Impl.Map.Insert (Key, Value);
      This.Impl.Keys.Append (Key);
   end Insert;

   ------------
   -- Insert --
   ------------

   function Insert (This    : Any;
                    Key     : Any;
                    Value   : Any;
                    Replace : Boolean := False)
                    return Any
   is
   begin
      return Result : Any := This do
         Result.Insert (Key, Value);
      end return;
   end Insert;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Any) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Any_Impl, Any_Impl_Ptr);
   begin
      Free (This.Impl);
   end Finalize;

   ----------------
   -- References --
   ----------------

   package body References is

      ----------
      -- Head --
      ----------

      function Head (This : Any) return Any
      is (Any (This.Impl.Vec.First_Element));

      ----------
      -- Tail --
      ----------

      function Tail (This : Any) return Any is
      begin
         return Result : Any := To_Any (Empty_Vec) do
            for I in This.Impl.Vec.First_Index + 1 .. This.Impl.Vec.Last_Index
            loop
               Result.Append (Any (This.Impl.Vec (I).Element.all));
            end loop;
         end return;
      end Tail;

      ----------
      -- Self --
      ----------

      function Self (This : Yeison_Generic.Any'Class) return Ref
      is (if This in Any
          then Any (This)'Unrestricted_Access
          else raise Program_Error with External_Tag (This'Tag));

      ----------
      -- Wrap --
      ----------

      function Wrap (This : Yeison_Generic.Any'Class) return Any
      is (if This in Any
          then Any (This)
          else To_Any (Yeison_Generic.Any (This)));
      pragma Unreferenced (Wrap);

      ---------
      -- Get --
      ---------

      function Get (This : Any; Pos : Any) return Any
      is (Reference (This, Pos).all);

      ---------------
      -- Reference --
      ---------------

      function Reference (This : Any; Pos : Any) return Ref
      is

         ----------------------
         -- Constraint_Error --
         ----------------------

         procedure Constraint_Error (Msg : String; Pos : Any'Class) is
         begin
            raise Standard.Constraint_Error
              with "cannot index " & Msg & " when index is "
              & Encode (Pos.Image);
         end Constraint_Error;

         -------------------
         -- Ref_By_Scalar --
         -------------------

         function Ref_By_Scalar (This : Any;
                                 Pos  : Any)
                                 return Ref
         is
            subtype Univ is Universal_Integer;
         begin

            --  Initialize empty vec/map if needed

            if This.Is_Nil then
               case Pos.Kind is
                  when Int_Kind =>
                     Self (This).all := To_Any (Empty_Vec);
                  when Map_Kind =>
                     Constraint_Error ("null Any with map", Pos);
                  when others =>
                     Self (This).all := To_Any (Empty_Map);
               end case;
            end if;

            --  Access the position. At this point Pos must be a scalar

            case This.Kind is
            when Scalar_Kinds =>
               --  Do not allow indexing an scalar at all
               Constraint_Error ("non-composite value", Pos);
               return null;

            when Map_Kind =>
               --  TODO: use cursors to avoid double lookup

               --  Put_Line ("pos: " & Pos.Image);
               --  Put_Line (This.Impl.Map.Contains (Pos)'Wide_Wide_Image);
               --  if This.Impl.Map.Contains (Pos) then
               --     Put_Line (This.Impl.Map (Pos).Image);
               --  end if;

               if not This.Impl.Map.Contains (Pos) then
                  This.Impl.Map.Insert (Pos, To_Any (Base.New_Nil));
               end if;

               return Self
                 (This.Impl.Map.Constant_Reference (Pos).Element.all);

            when others =>
               if Univ (This.Impl.Vec.Length) + 1 < To_Integer (Pos.As_Int)
               then
                  Constraint_Error
                    ("vector beyond 'length + 1 when 'length ="
                     & This.Impl.Vec.Length'Image, Pos);
               end if;

               if Univ (This.Impl.Vec.Length) < To_Integer (Pos.As_Int) then
                  This.Impl.Vec.Append (To_Any (Base.New_Nil));
               end if;

               return Self (This.Impl.Vec.Constant_Reference
                            (To_Integer (Pos.As_Int)).Element.all);
            end case;
         end Ref_By_Scalar;

      begin
         case Pos.Kind is
            when Nil_Kind =>
               Constraint_Error ("with null index", Pos);
               return null;

            when Map_Kind =>
               Constraint_Error ("with a map", Pos);
               return null;

            when Vec_Kind =>
               if Pos.Is_Empty then
                  raise Standard.Constraint_Error
                    with "cannot index with empty vector";
               elsif Pos.Length = 1 then
                  return Reference (This, Head (Pos));
               else
                  return Reference (Reference (This, Head (Pos)).all,
                                    Tail (Pos));
               end if;

            when Scalar_Kinds =>
               --  We can already return a reference
               return Ref_By_Scalar (This, Pos);

         end case;
      end Reference;

   end References;

end Yeison_Generic;
