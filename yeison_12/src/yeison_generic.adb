with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
--  with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Deallocation;
--  with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

--  with GNAT.IO; use GNAT.IO;

with Yeison_Utils;

package body Yeison_Generic is

   package Fixed renames Ada.Strings.Wide_Wide_Fixed;

   use all type Ada.Strings.Trim_End;

   subtype Any_Parent is Ada.Finalization.Controlled;

   package Any_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Any'Class, Any'Class,
                                                 "<" => Precedes);

   subtype Long_Long_Positive is
     Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   package Any_Vectors is
     new Ada.Containers.Indefinite_Vectors (Long_Long_Positive, Any'Class);

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

   ---------------
   -- Operators --
   ---------------

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L, R : Any) return Any is
      begin
         if L in Any_Scalar then
            return Result : Any := Empty_Vec do
               Result.Append (L);
               Result.Append (R);
            end return;
         elsif L in Any_Vec then
            return Result : Any := L do
               Result.Append (R);
            end return;
         else
            raise Constraint_Error with
              "Cannot append using ""/"" when left operator is: "
              & L.Kind'Image;
         end if;
      end "/";

   end Operators;

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

   -------------
   -- As_Text --
   -------------

   function As_Text (This : Any) return Text
   is (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (This.Impl.Str));

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

   ----------------
   -- JSON_Quote --
   ----------------

   function JSON_Quote (Str : Text) return Text is
   begin
      return '"' & Yeison_Utils.JSON_Escape (Str) & '"';
   end JSON_Quote;

   ---------
   -- Get --
   ---------

   function Get (This : Any; Pos : Any) return Any
   is (raise Unimplemented);

   -----------
   -- Image --
   -----------

   function Image (This    : Any'Class;
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

      function Scalar_Image (This : Any'Class) return Text
      is (case This.Kind is
             when Bool_Kind       =>
                (if This.Impl.Bool then "true" else "false"),
             when Int_Kind        =>
                Fixed.Trim (Image (This.Impl.Int), Side => Both),
             when Real_Kind       =>
                Fixed.Trim (Image (This.Impl.Real), Side => Both),
             when Str_Kind        =>
               (case Format is
                when Ada_Like => To_Wide_Wide_String (This.Impl.Str),
                when JSON => JSON_Quote (To_Wide_Wide_String (This.Impl.Str))),
             when Composite_Kinds =>
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
                             (if Contd then "" else Prefix) & Empty_Map_Image);
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & Map_Open
                          & (if Abbr then " " else NL));

                  while Has_Element (C) loop
                     Append (Result,
                             (if Abbr then " " else Prefix & Tab)
                             & Key (C).Image (Format, Compact)
                             & Map_Arrow);
                     --  TODO: the above key image should be prefixed in case
                     --  we are using an object for indexing.

                     Traverse (This.Impl.Map.Constant_Reference (C),
                               WS (Prefix & Tab
                                 & Key (C).Image (Format, Compact)
                                 & Map_Arrow),
                               Contd => True);
                     if not Abbr then
                        Append (Result, NL);
                     end if;
                     C := Next (C);
                  end loop;

                  Append (Result,
                          (if Abbr then " " else Prefix) & Map_Close);
               end;

            when Vec_Kind =>
               declare
                  Abbr : constant Boolean :=
                           Compact and then This.Impl.Vec.Length in 1;
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
                          (if Abbr then " " else Prefix)
                          & Vec_Close);
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

   ----------------
   -- References --
   ----------------

   package body References is

      ---------------
      -- Reference --
      ---------------

      function Reference (This : Any; Pos : Any) return Ref
      is

         ----------------------
         -- Constraint_Error --
         ----------------------

         procedure Constraint_Error (Msg : String; Pos : Any'Class) is
            use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
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
                                 return Ref is
         begin

            --  Initialize empty vec/map if needed

            if not This.Is_Valid then
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
               if Pos.Kind /= Int_Kind or else Pos.As_Int /= 1 then
                  Constraint_Error ("scalar value with any /= 1", Pos);
               end if;

               return Self (This);

            when Map_Kind =>
               --  TODO: use cursors to avoid double lookup

               if not This.Impl.Map.Contains (Pos) then
                  This.Impl.Map.Insert (Pos, To_Any (Invalid));
               end if;

               return Self
                 (This.Impl.Map.Constant_Reference (Pos).Element.all);

            when others =>
               if Long_Long_Integer (This.Impl.Vec.Length) + 1 < Pos.As_Int
               then
                  Constraint_Error
                    ("vector beyond 'length + 1 when 'length ="
                     & This.Impl.Vec.Length'Image, Pos);
               end if;

               if Long_Long_Integer (This.Impl.Vec.Length) < Pos.As_Int then
                  This.Impl.Vec.Append (To_Any (Invalid));
               end if;

               return Self (This.Impl.Vec.Constant_Reference
                            (Pos.As_Int).Element.all);
            end case;
         end Ref_By_Scalar;

      begin
         case Pos.Kind is
            when Map_Kind =>
               Constraint_Error ("with a map", Pos);
               return null;
            when Vec_Kind =>
               raise Unimplemented;
               --  Get first and continue indexing
            when Scalar_Kinds =>
               --  We can already return a reference
               return Ref_By_Scalar (This, Pos);
         end case;
      end Reference;

      ----------
      -- Self --
      ----------

      function Self (This : Yeison_Generic.Any'Class) return Ref
      is (if This in Any
          then Any (This)'Unrestricted_Access
          else Self (To_Any (Yeison_Generic.Any (This))));

   end References;

   ----------
   -- True --
   ----------

   function True return Any
   is (Any_Parent with
         Impl => new Any_Impl'
           (Kind => Bool_Kind,
            Bool => True));

end Yeison_Generic;
