package body Yeison is

   Unimplemented : exception;

   ------------
   -- To_Any --
   ------------

   function To_Any (This : Impl.Any) return Any
   is (This with null record);

   package References is new Impl.References (Any, To_Any) with Unreferenced;

   ---------------
   -- Operators --
   ---------------

   package body Operators is

      function "/" (L, R : Any) return Any
      is (Impl."/" (Impl.Any (L), Impl.Any (R)) with null record);

   end Operators;

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

   -------------
   -- To_Real --
   -------------

   function To_Real (Img : String) return Any
   is (raise Unimplemented);

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
   is (raise Unimplemented);

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Vec_Aux; Elem : Any) is
   begin
      This.Vec.Append (Elem);
   end Append;

end Yeison;
