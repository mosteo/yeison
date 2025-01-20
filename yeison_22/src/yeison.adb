with Ada.Unchecked_Conversion;

package body Yeison is

   Unimplemented : exception;

   ------------
   -- As_Ref --
   ------------

   function As_Ref (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

   ------------
   -- Insert --
   ------------

   procedure Insert (This  : in out Any;
                     Key   : Text;
                     Value : Any)
   is
   begin
      Impl.Any (This).Initialize (Key, Impl.Any (Value));
   end Insert;

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Any
   is (Make_Int (Big_Integers.From_String (Img)));

   ------------
   -- To_Str --
   ------------

   overriding
   function To_Str (Img : Text) return Any
   is (Impl.To_Str (Img) with null record);

   ---------------
   -- Const_Ref --
   ---------------

   function Const_Ref (This : aliased Any; Pos : Any) return Const
   is (raise Unimplemented);

   ---------------
   -- Reference --
   ---------------

   function Reference (This : aliased Any; Pos : Any) return Ref
   is
      T : Impl.Any renames Impl.Any (This);
      P : Impl.Any renames Impl.Any (Pos);

      type Any_Ptr is access all Any;
      type Impl_Ptr is access all Impl.Any;

      function Cast is
        new Ada.Unchecked_Conversion (Impl_Ptr, Any_Ptr);
   begin
      return Ref'(Element =>
                    Cast (T.Reference (P).Element).all'Unchecked_Access);
      --  This is safe to do because Any and Impl.Any are essentially the
      --  same type; we use a downgraded view of Any for referencing using the
      --  base type. Since the result cannot be downcasted (forbidden by Ada)
      --  despite us knowing it is right, we force-cast the pointers which has
      --  the same effect. Fingers crossed.
   end Reference;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Vec_Aux; Elem : Any'Class) is
   begin
      This.Vec.Append (Any (Elem));
   end Append;

end Yeison;
