with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Unchecked_Deallocation;

--  with Compare_To_Case;

package body Yeison_Generic is

   subtype Any_Parent is Ada.Finalization.Controlled;

   pragma Suppress (Container_Checks);
   package Any_Maps is
     new Ada.Containers.Ordered_Maps (Any, Any);

   subtype Long_Long_Positive is
     Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   package Any_Vectors is
     new Ada.Containers.Vectors (Long_Long_Positive, Any);

   type Any_Impl (Kind : Kinds := Bool_Kind) is record
      case Kind is
         when Bool_Kind =>
            Bool : Boolean;
         when Int_Kind =>
            Int  : Int_Type;
         when Real_Kind =>
            Real : Real_Type;
         when Str_Kind =>
            Str  : WWUString;
         when Map_Kind =>
            Map  : Any_Maps.Map;
         when Vec_Kind =>
            Vec  : Any_Vectors.Vector;
      end case;
   end record;

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

   function As_Bool (This : Any) return Boolean
   is (raise Unimplemented);

   ------------
   -- As_Int --
   ------------

   function As_Int (This : Any) return Long_Long_Integer
   is (To_Integer (This.Impl.Int));

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

   function Image (This    : Any;
                   Format  : Image_Formats := Ada_Like;
                   Compact : Boolean := False)
                   return Text
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      function "+" (S : Wide_Wide_String) return WWUString
                    renames To_Unbounded_Wide_Wide_String;

      ------------------
      -- Scalar_Image --
      ------------------

      function Scalar_Image (This : Any) return Text
      is (case This.Kind is
             when Bool_Kind       => This.Impl.Bool'Wide_Wide_Image,
             when Int_Kind        => Image (This.Impl.Int),
             when Real_Kind       => Image (This.Impl.Real),
             when Str_Kind        => To_Wide_Wide_String (This.Impl.Str),
             when Composite_Kinds =>
                raise Program_Error with "not a scalar: " & This.Kind'Image
         );

      Result : WWUString;

      --------------
      -- Traverse --
      --------------

      procedure Traverse (This   : Any;
                          Prefix : Text;
                          Contd  : Boolean := False)
      is
         NL  : constant Text := "" & Ada.Characters.Wide_Wide_Latin_1.LF;
         Tab : constant Text := "   ";
         function WS (Str : Text) return Text
         is (1 .. Str'Length => ' ');
      begin
         case This.Kind is
            when Scalar_Kinds =>
               Append (Result,
                       (if Contd then Text'("") else Prefix)
                       & Scalar_Image (This));

            when Map_Kind =>
               declare
                  C    : Any_Maps.Cursor := This.Impl.Map.First;
                  use Any_Maps;
                  Abbr : constant Boolean :=
                           Compact and then This.Impl.Map.Length in 1;
               begin
                  if This.Impl.Map.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & "[=>]");
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & "["
                          & (if Abbr then " " else NL));

                  while Has_Element (C) loop
                     Append (Result,
                             (if Abbr then " " else Prefix & Tab)
                             & Key (C).Image (Format, Compact) & " => ");
                     --  TODO: the above key image should be prefixed in case
                     --  we are using an object for indexing.

                     Traverse (This.Impl.Map.Constant_Reference (C),
                               WS (Prefix & Tab
                                 & Key (C).Image (Format, Compact)
                                 & " => "),
                               Contd => True);
                     if not Abbr then
                        Append (Result, NL);
                     end if;
                     C := Next (C);
                  end loop;

                  Append (Result,
                          (if Abbr then " " else Prefix) & "]");
               end;

            when Vec_Kind =>
               declare
                  Abbr : constant Boolean :=
                           Compact and then This.Impl.Vec.Length in 1;
                  I    : Natural := 0;
               begin
                  if This.Impl.Vec.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & "[,]");
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & "["
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
                          (if Abbr then " " else Prefix)
                          & "]");
               end;
         end case;
      end Traverse;

   begin
      if not This.Is_Valid then
         Result := +"(invalid)";
      else
         Traverse (This, "");
      end if;

      return To_Wide_Wide_String (Result);
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
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Any
   is (Any_Parent with Impl => new Any_Impl'
         (Kind => Str_Kind,
          Str  => +Img));

   --------------
   -- Make_Int --
   --------------

   function Make_Int (This : Int_Type) return Any
   is (Any_Parent with Impl => new Any_Impl'
         (Kind => Int_Kind,
          Int  => This));

   --------------
   -- Make_Str --
   --------------

   function Make_Str (This : Text) return Any renames To_Str;

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
      This.Impl.Map.Insert (Key, Value);
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
         use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      begin
         raise Standard.Constraint_Error
           with "cannot index " & Msg & " when index is "
           & Encode (Pos.Image);
      end Constraint_Error;

      -------------------
      -- Ref_By_Scalar --
      -------------------

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
               if Long_Long_Integer (This.Impl.Vec.Length) + 1 < Pos.As_Int
               then
                  Constraint_Error
                    ("vector beyond 'length + 1 when 'length ="
                     & This.Impl.Vec.Length'Image, Pos);
               end if;

               if Long_Long_Integer (This.Impl.Vec.Length) < Pos.As_Int then
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

   function True return Any
   is (Any_Parent with
         Impl => new Any_Impl'
           (Kind => Bool_Kind,
            Bool => True));

end Yeison_Generic;
