with Ada.Characters.Conversions;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Compiler_Version;

package body Yeison_12 is

   package Fixed renames Ada.Strings.Wide_Wide_Fixed;
   package Compiler_Version is new GNAT.Compiler_Version;

   use type Ada.Containers.Count_Type;
   use all type Ada.Strings.Trim_End;

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

   ----------------------------------------------------------------------------
   --  Scalars  --------------------------------------------------------------
   ----------------------------------------------------------------------------

   function Kind (This : Scalar) return Scalar_Kinds is (This.Data.Kind);

   function As_Boolean (This : Scalar) return Boolean is (This.Data.Bool);
   function As_Integer (This : Scalar) return Big_Int is (This.Data.Int);
   function As_Real (This : Scalar) return Reals.General_Real
   is (This.Data.Real);
   function As_Text (This : Scalar) return Text is (S (This.Data.Str));

   function Image (This   : Scalar;
                   Format : Image_Formats := Ada_Like) return Text
   is (Make.Scalar (This).Image (Format));

   package body Scalars is

      function New_Bool (Val : Boolean) return Scalar
      is (Data => (Kind => Bool_Kind, Bool => Val));

      function New_Int (Val : Big_Int) return Scalar
      is (Data => (Kind => Int_Kind, Int => Val));

      function New_Real (Val : Reals.General_Real) return Scalar
      is (Data => (Kind => Real_Kind, Real => Val));

      function New_Text (Val : Text) return Scalar
      is (Data => (Kind => Str_Kind, Str => U (Val)));

   end Scalars;

   ----------------------------------------------------------------------------
   --  Implementation primitives  --------------------------------------------
   ----------------------------------------------------------------------------

   function Nil_Impl return Any_Impl_Ptr
   is (new Any_Impl'(Kind => Nil_Kind));

   ----------
   -- Kind --
   ----------

   function Kind (This : Any) return Kinds is (This.Impl.Kind);

   ----------------
   -- Scalar "<" --
   ----------------

   function "<" (L, R : Any_Impl) return Boolean is
      use type WWUString;
      use type Reals.General_Real;

      ------------------
      -- Compare_Maps --
      ------------------

      function Compare_Maps return Boolean is
         L_Cursor : Any_Maps.Cursor := L.Map.First;
         R_Cursor : Any_Maps.Cursor := R.Map.First;
      begin
         while Any_Maps.Has_Element (L_Cursor)
           and then Any_Maps.Has_Element (R_Cursor)
         loop
            declare
               L_Key : Any renames Any_Maps.Key (L_Cursor);
               R_Key : Any renames Any_Maps.Key (R_Cursor);
            begin
               if L_Key < R_Key then
                  return True;
               elsif R_Key < L_Key then
                  return False;
               end if;

               declare
                  L_Value : Any renames Any_Maps.Element (L_Cursor);
                  R_Value : Any renames Any_Maps.Element (R_Cursor);
               begin
                  if L_Value < R_Value then
                     return True;
                  elsif R_Value < L_Value then
                     return False;
                  end if;
               end;
            end;

            Any_Maps.Next (L_Cursor);
            Any_Maps.Next (R_Cursor);
         end loop;

         --  If L is shorter than R, it is less
         return Any_Maps.Has_Element (R_Cursor);
      end Compare_Maps;

      ------------------
      -- Compare_Vecs --
      ------------------

      function Compare_Vecs return Boolean is
      begin
         for I in L.Vec.First_Index .. L.Vec.Last_Index loop
            exit when I > R.Vec.Last_Index;
            declare
               L_Elem : Any renames L.Vec (I);
               R_Elem : Any renames R.Vec (I);
            begin
               if L_Elem < R_Elem then
                  return True;
               elsif R_Elem < L_Elem then
                  return False;
               end if;
            end;
         end loop;

         return L.Vec.Last_Index < R.Vec.Last_Index;
      end Compare_Vecs;

   begin
      if L.Kind < R.Kind then
         return True;
      elsif R.Kind < L.Kind then
         return False;
      end if;

      --  Both the same kind

      case L.Kind is
         when Nil_Kind  => return False;
         when Bool_Kind => return L.Val.Bool < R.Val.Bool;
         when Int_Kind  => return L.Val.Int < R.Val.Int;
         when Real_Kind => return L.Val.Real < R.Val.Real;
         when Str_Kind  => return L.Val.Str < R.Val.Str;
         when Map_Kind  => return Compare_Maps;
         when Vec_Kind  => return Compare_Vecs;
      end case;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Any) return Boolean
   is (L.Impl.all = R.Impl.all);

   function "=" (L : Any; R : Text) return Boolean
   is (L.Kind = Str_Kind and then L.As_Text = R);

   function "=" (L : Text;    R : Any)     return Boolean is (R = L);
   function "=" (L : Any;     R : Big_Int) return Boolean
   is (L.Kind = Int_Kind and then L.As_Int = R);
   function "=" (L : Big_Int;  R : Any)     return Boolean is (R = L);

   function "=" (L : Any;      R : Big_Real) return Boolean is
      use type Reals.General_Real;
   begin
      return L.Kind = Real_Kind and then L.As_Real = Reals.New_Real (R);
   end "=";
   function "=" (L : Big_Real; R : Any)      return Boolean is (R = L);

   function Is_True  (This : Any) return Boolean is (This.As_Bool);
   function Is_False (This : Any) return Boolean is (not This.As_Bool);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Any) return Boolean is
   begin
      if L.Kind < R.Kind then
         return True;
      elsif R.Kind < L.Kind then
         return False;
      else
         return L.Impl.all < R.Impl.all;
      end if;
   end "<";

   ----------------
   -- Retrieval  --
   ----------------

   function As_Scalar (This : Any'Class) return Scalar
   is (Scalar'(Data => This.Impl.Val));

   function As_Bool (This : Any) return Boolean is (This.Impl.Val.Bool);

   function As_Int (This : Any) return Big_Int is (This.Impl.Val.Int);

   function As_Real (This : Any) return Reals.General_Real
   is (This.Impl.Val.Real);

   function As_Real_Float (This : Any) return Big_Real is
      use type Reals.Classes;
      General : Reals.General_Real renames This.Impl.Val.Real;
   begin
      if General.Class /= Reals.Finite then
         raise Constraint_Error
           with "non-finite real value has no Big_Real representation";
      end if;
      return General.Value;
   end As_Real_Float;

   ---------
   -- "-" --
   ---------

   function "-" (Right : Any) return Any is
   begin
      case Right.Kind is
         when Int_Kind  => return Make.Int (-Right.As_Int);
         when Real_Kind => return Make.Real (Reals.Negate (Right.As_Real));
         when others    =>
            raise Constraint_Error
              with "cannot negate a " & Right.Kind'Image & " value";
      end case;
   end "-";

   function As_Text (This : Any) return Text is (S (This.Impl.Val.Str));

   function As_UTF_8 (This : Any) return String
   is (Yeison_Utils.Encode (This.As_Text));

   function As_Latin_1 (This : Any) return String
   is (Ada.Characters.Conversions.To_String (This.As_Text));

   ---------------
   -- Empty_Map --
   ---------------

   function Empty_Map return Any
   is (Any_Parent with Impl => new Any_Impl'(Kind => Map_Kind, others => <>));

   ---------------
   -- Empty_Vec --
   ---------------

   function Empty_Vec return Any
   is (Any_Parent with Impl => new Any_Impl'(Kind => Vec_Kind, others => <>));

   -----------------
   -- First_Index --
   -----------------

   function First_Index (This : Any) return Universal_Integer
   is (Universal_Integer (This.Impl.Vec.First_Index));

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (This : Any) return Universal_Integer
   is (Universal_Integer (This.Impl.Vec.Last_Index));

   -----------
   -- Image --
   -----------

   function Image (This    : Any'Class;
                   Format  : Image_Formats := Ada_Like;
                   Options : Image_Options := (others => <>))
                   return Text
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      ------------------
      -- Scalar_Image --
      ------------------

      function Scalar_Image (This : Any'Class) return Text
      is (case This.Kind is
             when Bool_Kind       =>
                (if This.Impl.Val.Bool then "true" else "false"),
             when Int_Kind        =>
                Fixed.Trim (Big_Int'Wide_Wide_Image (This.Impl.Val.Int),
                            Side => Both),
             when Real_Kind       =>
                Fixed.Trim (Reals.Image (This.Impl.Val.Real), Side => Both),
             when Str_Kind        =>
               (case Format is
                   when Ada_Like =>
                      To_Wide_Wide_String (This.Impl.Val.Str),
                   when JSON     =>
                      Yeison_Utils.JSON_Quote
                        (To_Wide_Wide_String (This.Impl.Val.Str))),
             when Nonscalar_Kinds =>
                raise Program_Error with "not a scalar: " & This.Kind'Image
         );

      Result : WWUString;

      function Empty_Map_Image return Text
      is (case Format is when Ada_Like => "(=>)", when JSON => "{}");

      function Map_Open return Text
      is (case Format is when Ada_Like => "(", when JSON => "{");

      function Map_Close return Text
      is (case Format is when Ada_Like => ")", when JSON => "}");

      function Map_Arrow return Text
      is (case Format is when Ada_Like => " => ", when JSON => ": ");

      function Empty_Vec_Image return Text
      is (case Format is when Ada_Like => "()", when JSON => "[]");

      function Vec_Open return Text
      is (case Format is when Ada_Like => "(", when JSON => "[");

      function Vec_Close return Text
      is (case Format is when Ada_Like => ")", when JSON => "]");

      --------------
      -- Traverse --
      --------------

      procedure Traverse (This   : Any'Class;
                          Prefix : Text;
                          Contd  : Boolean := False)
      is
         NL  : constant Text := "" & Ada.Characters.Wide_Wide_Latin_1.LF;
         Tab : constant Text :=
                 (case Format is when Ada_Like => "   ", when JSON => "  ");
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
                     Traverse (E, Prefix & Tab, Contd => Abbr);
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
          when Nil_Kind => True,
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

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (This : in out Any) is
   begin
      This.Impl := new Any_Impl'(This.Impl.all);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Any) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Any_Impl, Any_Impl_Ptr);
   begin
      Free (This.Impl);
   exception
      when others =>
         --  GNAT 12/13 has a finalization bug; suppress spurious exceptions
         --  raised during deallocation when built with that compiler. This is
         --  probably a bad idea, it would be better to avoid that compiler
         --  entirely...
         declare
            V : constant String := Compiler_Version.Version;
         begin
            if V'Length < 2 or else
              (V (V'First .. V'First + 1) /= "12" and then
               V (V'First .. V'First + 1) /= "13")
            then
               raise;
            end if;
         end;
   end Finalize;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Any; Elem : Any) is
   begin
      This.Impl.Vec.Append (Elem);
   end Append;

   function Append (This : Any; Elem : Any) return Any is
   begin
      return Result : Any := This do
         Result.Append (Elem);
      end return;
   end Append;

   procedure Append (This : in out Any; Str : Text) is
   begin
      This.Append (Make.Str (Str));
   end Append;

   function Append (This : Any; Str : Text) return Any
   is (This.Append (Make.Str (Str)));

   -----------------
   -- Insert_Impl --
   -----------------

   procedure Insert_Impl (Impl    : in out Any_Impl;
                          Key     : Any;
                          Value   : Any;
                          Replace : Boolean := False)
   is
   begin
      if Replace and then Impl.Map.Contains (Key) then
         Impl.Map.Replace (Key, Value);
         --  The key already exists, no need to update the Keys vector
      else
         Impl.Map.Insert (Key, Value);
         Impl.Keys.Append (Key);
      end if;
   end Insert_Impl;

   ------------
   -- Insert --
   ------------

   procedure Insert (This    : in out Any;
                     Key     : Any;
                     Value   : Any;
                     Replace : Boolean := False)
   is
   begin
      Insert_Impl (This.Impl.all, Key, Value, Replace);
   end Insert;

   function Insert (This    : Any;
                    Key     : Any;
                    Value   : Any;
                    Replace : Boolean := False)
                    return Any
   is
   begin
      return Result : Any := This do
         Result.Insert (Key, Value, Replace);
      end return;
   end Insert;

   ----------
   -- Keys --
   ----------

   function Keys (This : Any; Ordered : Boolean := False) return Any is
   begin
      return Result : Any := Empty_Vec do
         if Ordered then
            for I in This.Impl.Map.Iterate loop
               Result.Append (Any_Maps.Key (I));
            end loop;
         else
            for Key of This.Impl.Keys loop
               Result.Append (Key);
            end loop;
         end if;
      end return;
   end Keys;

   -------------
   -- Has_Key --
   -------------

   function Has_Key (This : Any; Key : Any) return Boolean
   is (This.Impl.Map.Contains (Key));

   function Has_Key (This : Any; Key : UTF_8_String) return Boolean
   is (This.Has_Key (Make.Str (Yeison_Utils.Decode (Key))));

   ----------
   -- Head --
   ----------

   function Head (This : Any) return Any
   is (This.Impl.Vec.First_Element);

   ----------
   -- Tail --
   ----------

   function Tail (This : Any) return Any is
   begin
      if This.Is_Empty then
         raise Constraint_Error with "Tail of empty vector";
      end if;
      return Result : Any := Empty_Vec do
         for I in This.Impl.Vec.First_Index + 1 .. This.Impl.Vec.Last_Index
         loop
            Result.Append (This.Impl.Vec (I));
         end loop;
      end return;
   end Tail;

   -------------
   -- Resolve --
   -------------

   --  Single position resolver shared by Reference (read/write) and
   --  Constant_Reference (read-only). It returns an access to the designated
   --  element. When Create, missing positions are materialized: a nil target
   --  is auto-vivified into the proper container, a vector is grown by exactly
   --  one past its end, and a missing map key is inserted as nil. When not
   --  Create, any invalid index/key raises Constraint_Error and This is left
   --  untouched.

   function Resolve (This   : Any;
                     Pos    : Any;
                     Create : Boolean) return not null access Any
   is
      Target : constant not null access Any := This'Unrestricted_Access;

      ----------------------
      -- Constraint_Error --
      ----------------------

      procedure Constraint_Error (Msg : String) is
      begin
         raise Standard.Constraint_Error
           with "cannot index " & Msg & " when index is "
           & Yeison_Utils.Encode (Pos.Image);
      end Constraint_Error;

   begin
      case Pos.Kind is
         when Nil_Kind =>
            Constraint_Error ("with null index");

         when Map_Kind =>
            Constraint_Error ("with a map");

         when Vec_Kind =>
            --  Nested indexing: consume one position at a time
            if Pos.Is_Empty then
               raise Standard.Constraint_Error
                 with "cannot index with empty vector";
            elsif Pos.Length = 1 then
               return Resolve (Target.all, Head (Pos), Create);
            else
               return Resolve (Resolve (Target.all, Head (Pos), Create).all,
                               Tail (Pos), Create);
            end if;

         when Scalar_Kinds =>
            --  Auto-vivify a nil target (only when creating)
            if Target.Is_Nil and then Create then
               if Pos.Kind = Int_Kind then
                  Target.all := Empty_Vec;
               else
                  Target.all := Empty_Map;
               end if;
            end if;

            case Target.Kind is
               when Map_Kind =>
                  if not Target.Impl.Map.Contains (Pos) then
                     if Create then
                        Insert_Impl (Target.Impl.all, Pos, Make.Nil);
                     else
                        Constraint_Error ("a map without the key");
                     end if;
                  end if;

                  return Target.Impl.Map.Reference (Pos).Element.all
                           'Unrestricted_Access;

               when Vec_Kind =>
                  if Pos.Kind /= Int_Kind then
                     Constraint_Error ("a vector with a non-integer index");
                  end if;

                  declare
                     Index : constant Universal_Integer := Pos.As_Int;
                     Len   : constant Universal_Integer :=
                               Universal_Integer (Target.Impl.Vec.Length);
                  begin
                     if Index <= 0 then
                        Constraint_Error
                          ("a vector with non-positive index " & Index'Image);
                     elsif Create then
                        if Index > Len + 1 then
                           Constraint_Error
                             ("a vector beyond 'length + 1 when 'length ="
                              & Len'Image);
                        elsif Index = Len + 1 then
                           Target.Impl.Vec.Append (Make.Nil);
                        end if;
                     elsif Index > Len then
                        Constraint_Error ("a vector out of range");
                     end if;

                     return Target.Impl.Vec.Reference (Index).Element.all
                              'Unrestricted_Access;
                  end;

               when Nil_Kind | Scalar_Kinds =>
                  Constraint_Error ("a non-composite value");
            end case;
      end case;

      --  Unreachable: every branch above either returns or raises.
      raise Program_Error;
   end Resolve;

   ---------------
   -- Reference --
   ---------------

   function Reference (This : in out Any; Pos : Any) return Ref
   is (Ref'(Element => Resolve (This, Pos, Create => True)));

   function Reference (This : in out Any; Pos : UTF_8_String) return Ref
   is (This.Reference (Make.Str (Yeison_Utils.Decode (Pos))));

   function Reference (This : in out Any; Pos : Big_Int) return Ref
   is (This.Reference (Make.Int (Pos)));

   ---------
   -- Get --
   ---------

   function Get (This : Any; Pos : Any) return Any
   is (Resolve (This, Pos, Create => False).all);

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference (This : Any; Pos : Any) return Const
   is (Const'(Element => Resolve (This, Pos, Create => False)));

   function Constant_Reference (This : Any; Pos : UTF_8_String) return Const
   is (This.Constant_Reference (Make.Str (Yeison_Utils.Decode (Pos))));

   function Constant_Reference (This : Any; Pos : Big_Int) return Const
   is (This.Constant_Reference (Make.Int (Pos)));

   ----------------------------------------------------------------------------
   --  Iteration  ------------------------------------------------------------
   ----------------------------------------------------------------------------

   ------------------
   -- First_Cursor --
   ------------------

   function First_Cursor (This : Any) return Cursor is
   begin
      case This.Kind is
         when Map_Kind =>
            if This.Impl.Map.Is_Empty then
               return (Kind => Invalid);
            else
               return (Kind => Map_Cursor, Map_Pos => This.Impl.Map.First);
            end if;
         when Vec_Kind =>
            if This.Impl.Vec.Is_Empty then
               return (Kind => Invalid);
            else
               return (Kind => Vec_Cursor, Vec_Pos => This.Impl.Vec.First);
            end if;
         when others =>
            return (Kind => Invalid);
      end case;
   end First_Cursor;

   -----------------
   -- Next_Cursor --
   -----------------

   function Next_Cursor (Pos : Cursor) return Cursor is
   begin
      case Pos.Kind is
         when Map_Cursor =>
            declare
               Next_Pos : Any_Maps.Cursor := Pos.Map_Pos;
            begin
               Any_Maps.Next (Next_Pos);
               if Any_Maps.Has_Element (Next_Pos) then
                  return (Kind => Map_Cursor, Map_Pos => Next_Pos);
               else
                  return (Kind => Invalid);
               end if;
            end;
         when Vec_Cursor =>
            declare
               Next_Pos : Any_Vecs.Cursor := Pos.Vec_Pos;
            begin
               Any_Vecs.Next (Next_Pos);
               if Any_Vecs.Has_Element (Next_Pos) then
                  return (Kind => Vec_Cursor, Vec_Pos => Next_Pos);
               else
                  return (Kind => Invalid);
               end if;
            end;
         when Invalid =>
            return Pos;
      end case;
   end Next_Cursor;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Pos : Cursor) return Boolean
   is (Pos.Kind /= Invalid);

   -----------
   -- First --
   -----------

   function First (This : Any) return Cursor
   is (First_Cursor (This));

   overriding function First (Object : Iterator) return Cursor
   is (First_Cursor (Object.Container.all));

   -------------
   -- Element --
   -------------

   function Element (This : Any; Pos : Cursor) return Any
   is (Constant_Reference (This, Pos).Element.all);

   ---------
   -- Key --
   ---------

   function Key (This : Any; Pos : Cursor) return Any is
      pragma Unreferenced (This);
   begin
      case Pos.Kind is
         when Map_Cursor =>
            return Any_Maps.Key (Pos.Map_Pos);
         when Vec_Cursor =>
            raise Constraint_Error with "Key called on a vector cursor";
         when Invalid =>
            raise Constraint_Error with "Key called on an invalid cursor";
      end case;
   end Key;

   ----------
   -- Next --
   ----------

   overriding function Next (Object   : Iterator;
                             Position : Cursor) return Cursor
   is (Next_Cursor (Position));

   -------------
   -- Iterate --
   -------------

   function Iterate (This : Any) return Iteration.Forward_Iterator'Class
   is (Iterator'(Container => This'Unrestricted_Access));

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference (This : Any; Pos : Cursor) return Const is
   begin
      case Pos.Kind is
         when Map_Cursor =>
            --  For maps, we return just the value part, discarding the key
            return Const'(Element =>
              This.Impl.Map.Constant_Reference (Pos.Map_Pos).Element);
         when Vec_Cursor =>
            return Const'(Element =>
              This.Impl.Vec.Constant_Reference (Pos.Vec_Pos).Element);
         when Invalid =>
            raise Constraint_Error with "invalid cursor";
      end case;
   end Constant_Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference (This : in out Any; Pos : Cursor) return Ref is
   begin
      case Pos.Kind is
         when Map_Cursor =>
            return Ref'(Element =>
              This.Impl.Map.Reference (Pos.Map_Pos).Element.all
              'Unrestricted_Access);
         when Vec_Cursor =>
            return Ref'(Element =>
              This.Impl.Vec.Reference (Pos.Vec_Pos).Element.all
              'Unrestricted_Access);
         when Invalid =>
            raise Constraint_Error with "invalid cursor";
      end case;
   end Reference;

   ----------------------------------------------------------------------------
   --  Make  -----------------------------------------------------------------
   ----------------------------------------------------------------------------

   package body Make is

      function Nil return Any
      is (Any_Parent with Impl => Nil_Impl);

      function Scalar (This : Yeison_12.Scalar) return Any
      is (case This.Data.Kind is
             when Bool_Kind => Bool (This.Data.Bool),
             when Int_Kind  => Int  (This.Data.Int),
             when Real_Kind => Real (This.Data.Real),
             when Str_Kind  => Str  (S (This.Data.Str)));

      function False return Any is (Bool (Standard.False));
      function True  return Any is (Bool (Standard.True));

      function Bool (This : Boolean) return Any
      is (Any_Parent with Impl =>
             new Any_Impl'(Bool_Kind, (Bool_Kind, This)));

      function Int (This : Big_Int) return Any
      is (Any_Parent with Impl =>
             new Any_Impl'(Int_Kind, (Int_Kind, This)));

      function Real (This : Reals.General_Real) return Any
      is (Any_Parent with Impl =>
             new Any_Impl'(Real_Kind, (Real_Kind, This)));

      function Real (This : Big_Real) return Any
      is (Real (Reals.New_Real (This)));

      function Str (This : Text) return Any
      is (Any_Parent with Impl =>
             new Any_Impl'(Str_Kind, (Str_Kind, U (This))));

      function Map return Any renames Yeison_12.Empty_Map;
      function Vec return Any renames Yeison_12.Empty_Vec;

   end Make;

   ----------------------------------------------------------------------------
   --  Operators  ------------------------------------------------------------
   ----------------------------------------------------------------------------

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L, R : Any) return Any is
      begin
         if L.Kind in Scalar_Kinds then
            return Result : Any := Empty_Vec do
               Result.Append (L);
               Result.Append (R);
            end return;
         elsif L.Kind in Vec_Kind then
            return Result : Any := L do
               Result.Append (R);
            end return;
         else
            raise Constraint_Error with
              "Cannot append using ""/"" when left operator is: "
              & L.Kind'Image;
         end if;
      end "/";

      ------------
      -- To_Vec --
      ------------

      function To_Vec (This : Any_Array) return Any is
      begin
         return Result : Any := Empty_Vec do
            for Elem of This loop
               Result.Append (Elem);
            end loop;
         end return;
      end To_Vec;

   end Operators;

end Yeison_12;
