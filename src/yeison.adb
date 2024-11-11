with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

--  with Compare_To_Case;

package body Yeison is

   subtype Any_Parent is Ada.Finalization.Controlled;

   package Any_Maps is
     new Ada.Containers.Ordered_Maps (Any, Any);

   package Any_Vectors is
      new Ada.Containers.Vectors (Positive, Any);

   type Any_Impl (Kind : Kinds := Bool_Kind) is record
      case Kind is
         when Bool_Kind =>
            Bool : Boolean;
         when Int_Kind =>
            Int  : Big_Int;
         when Real_Kind =>
            Real : Big_Real;
         when Str_Kind =>
            Str : WWUString;
         when Map_Kind =>
            Map : Any_Maps.Map;
         when Vec_Kind =>
            Vec : Any_Vectors.Vector;
      end case;
   end record;

   ----------
   -- Make --
   ----------

   package body Make is

      ---------
      -- Vec --
      ---------

      function Vec (This : Yeison.Vec) return Any is (Any (This));

   end Make;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Any_Impl) return Boolean is
      use type Big_Int;
      use type Big_Real;
      use type WWUString;
   begin
      if L.Kind < R.Kind then
         return True;
      elsif R.Kind < L.Kind then
         return False;
      end if;

      --  Both the same

      case L.Kind is
         when Bool_Kind => return L.Bool < R.Bool;
         when Int_Kind  => return L.Int < R.Int;
         when Real_Kind => return L.Real < R.Real;
         when Str_Kind  => return L.Str < R.Str;
         when Map_Kind  => raise Unimplemented;
         when Vec_Kind  => raise Unimplemented;
      end case;
   end "<";

   -------------
   -- As_Bool --
   -------------

   function As_Bool return Boolean
   is (raise Unimplemented);

   ------------
   -- As_Int --
   ------------

   function As_Int (This : Any) return Integer
   is (Big_Ints.To_Integer (This.Impl.Int));

   -------------------
   -- Empty_Any_Vec --
   -------------------

   function Empty_Any_Vec return Vec is (Any'(Empty_Vec) with null record);

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

   -----------
   -- False --
   -----------

   function False return Any is (raise Unimplemented);

   -----------
   -- Image --
   -----------

   function Image (This : Any) return String is
      use Ada.Strings.Unbounded;
      function "+" (S : String) return Unbounded_String
                    renames To_Unbounded_String;

      Result : Unbounded_String;

      --------------
      -- Traverse --
      --------------

      procedure Traverse (This   : Node'Class;
                          Prefix : String;
                          Contd  : Boolean := False)
      is
         NL  : constant Character := ASCII.LF;
         Tab : constant String := "   ";
         function WS (Str : String) return String
         is (1 .. Str'Length => ' ');
      begin
         case This.Kind is
            when Atom_Kind =>
               for E of This loop -- Only one, but we test the iterator so
                  Append (Result,
                          (if Contd then "" else Prefix) & Image (E.Get));
               end loop;

            when Dict_Kind =>
               declare
                  Real : Real_Node renames Real_Node (This.Ptr.all);
                  C    : Node_Maps.Cursor := Real.Data.Dict.First;
                  use Node_Maps;
                  Abbr : constant Boolean :=
                           Compact and then Real.Data.Dict.Length in 1;
               begin
                  if Real.Data.Dict.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & "{}");
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & "{"
                          & (if Abbr then ' ' else NL));

                  while Has_Element (C) loop
                     Append (Result,
                             (if Abbr then " " else Prefix & Tab)
                             & Key (C) & " : ");
                     Traverse (Real.Data.Dict.Reference (C).Ref,
                               WS (Prefix & Tab & Key (C) & " : "),
                               Contd => True);
                     if not Abbr then
                        Append (Result, NL);
                     end if;
                     C := Next (C);
                  end loop;

                  Append (Result,
                          (if Abbr then " " else Prefix) & "}");
               end;

            when List_Kind =>
               declare
                  Real : Real_Node renames Real_Node (This.Ptr.all);
                  Abbr : constant Boolean :=
                           Compact and then Real.Data.List.Length in 1;
                  I    : Natural := 0;
               begin
                  if Real.Data.List.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & "[]");
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & "["
                          & (if Abbr then ' ' else NL));

                  for E of This loop
                     Traverse (E,
                               Prefix & Tab,
                               Contd => Abbr);
                     I := I + 1;
                     Append (Result,
                             (if I = Natural (Real.Data.List.Length)
                              then ""
                              else ",")
                             & (if Abbr then ' ' else NL));
                  end loop;
                  Append (Result,
                          (if Abbr then " " else Prefix)
                          & "]");
               end;
         end case;
      end Traverse;

   begin
      if This.Is_Empty then
         Result := +"(empty)";
      else
         Traverse (This.R.Root.Constant_Reference.Ref, "");
      end if;

      return To_String (Result);
   end Image;

   -------------
   -- Invalid --
   -------------

   function Invalid return Any
   is (Ada.Finalization.Controlled with Impl => null);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Any) return Boolean
   is (This.Impl /= null);

   ----------
   -- Kind --
   ----------

   function Kind (This : Any) return Kinds
   is (This.Impl.Kind);

   ----------
   -- Self --
   ----------

   function Self (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Any
   is (Any_Parent with
       Impl => new Any_Impl'
        (Kind => Int_Kind,
         Int  => Big_Ints.From_String (Img)));

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Any
   is (Any_Parent with Impl => new Any_Impl'
        (Kind => Str_Kind,
         Str  => +Img));

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Any) return Boolean is
   begin
      if not L.Is_Valid and then R.Is_Valid then
         return True;
      elsif not R.Is_Valid and then L.Is_Valid then
         return False;
      elsif not L.Is_Valid and then not R.Is_Valid then
         return False;
      end if;

      --  Both valid

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
      if This.Is_Valid then
         This.Impl := new Any_Impl'(This.Impl.all);
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Any; Elem : Any) is
   begin
      This.Impl.Vec.Append (Elem);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Vec;
                     Elem : Any'Class)
   is
   begin
      Any (This).Append (Any (Elem));
   end Append;

   ---------------
   -- Const_Ref --
   ---------------

   function Const_Ref (This : aliased Any; Pos : Any) return Const
   is (raise Unimplemented);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This  : in out Any;
                         Key   : Text;
                         Value : Any)
   is
   begin
      This.Impl.Map.Insert (To_Str (Key), Value);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert (This    : in out Any;
                     Key     : Any;
                     Value   : Any;
                     Replace : Boolean := False)
   is
   begin
      raise Unimplemented;
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

   ---------------
   -- Reference --
   ---------------

   function Reference (This : aliased Any; Pos : Any) return Ref
   is

      procedure Constraint_Error (Msg : String; Pos : Any) with No_Return;

      ----------------------
      -- Constraint_Error --
      ----------------------

      procedure Constraint_Error (Msg : String; Pos : Any) is
      begin
         raise Standard.Constraint_Error
           with "cannot index " & Msg & " when index is " & Pos.Image;
      end Constraint_Error;

      ------------------
      -- Ref_By_Index --
      ------------------

      function Ref_By_Scalar (This : aliased Any; Pos : Any) return Ref is
      begin
         --  Initialize empty vec/map if needed
         if not This.Is_Valid then
            case Pos.Kind is
               when Int_Kind =>
                  This.Self := Empty_Vec;
               when Map_Kind =>
                  Constraint_Error ("null Any with map", Pos);
               when others =>
                  This.Self := Empty_Map;
            end case;
         end if;

         --  Access the position. At this point Pos must be a scalar

         case This.Kind is
            when Scalar_Kinds =>
               if Pos.Kind /= Int_Kind or else Pos.As_Int /= 1 then
                  Constraint_Error ("scalar value with any /= 1", Pos);
               end if;

               return This.Self;

            when Map_Kind =>
               --  TODO: use cursors to avoid double lookup

               if not This.Impl.Map.Contains (Pos) then
                  This.Impl.Map.Insert (Pos, Invalid);
               end if;

               return This.Impl.Map.Constant_Reference (Pos).Self;

            when others =>
               if Integer (This.Impl.Vec.Length) + 1 < Pos.As_Int then
                  Constraint_Error
                    ("vector beyond 'length + 1 when 'length ="
                    & This.Impl.Vec.Length'Image, Pos);
               end if;

               if Integer (This.Impl.Vec.Length) < Pos.As_Int then
                  This.Impl.Vec.Append (Invalid);
               end if;

               return This.Impl.Vec.Constant_Reference (Pos.As_Int).Self;
         end case;
      end Ref_By_Scalar;

   begin
      case Pos.Kind is
         when Map_Kind =>
            Constraint_Error ("with a map", Pos);
         when Vec_Kind =>
            raise Unimplemented;
            --  Get first and continue indexing
         when Scalar_Kinds =>
            --  We can already return a reference
            return Ref_By_Scalar (This, Pos);
      end case;
   end Reference;

   ----------
   -- True --
   ----------

   function True return Any is (raise Unimplemented);

end Yeison;
