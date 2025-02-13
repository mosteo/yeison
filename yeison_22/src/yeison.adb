package body Yeison is

   ------------
   -- To_Any --
   ------------

   function To_Any (This : Impl.Any) return Any
   is (This with null record);

   package Operators is new Impl.Operators (Any);

   package References is new Impl.References (Any);

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
      This.Insert (To_Str (Key), Value, Replace => False);
   end Insert;

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Any
   is (Operators.Make.Int (Big_Integers.From_String (Img)));

   -------------
   -- To_Real --
   -------------

   function To_Real (Img : String) return Any
   is (Operators.Make.Real (Big_Reals.From_String (Img)));

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Text) return Any renames Operators.Make.Str;

   ---------------
   -- Const_Ref --
   ---------------

   function Const_Ref (This : aliased Any; Pos : Any) return Const
   is (Element => References.Reference (This, Pos));

   ---------
   -- Get --
   ---------

   function Get (This, Pos : Any) return Any
   renames References.Get;

   ---------------
   -- Reference --
   ---------------

   function Reference (This : aliased Any; Pos : Any) return Ref
   is (Element => References.Reference (This, Pos));

end Yeison;
