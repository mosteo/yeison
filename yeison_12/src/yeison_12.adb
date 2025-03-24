package body Yeison_12 is

   ------------
   -- To_Any --
   ------------

   function To_Any (This : Impl.Any) return Any
   is (This with null record);

   package References is new Impl.References (Any, To_Any);

   -------------
   -- Has_Key --
   -------------

   function Has_Key (This : Any; Key : UTF_8_String) return Boolean
   is (References.Has_Key (This, Make.Str (Yeison_Utils.Decode (Key))));

   ---------------
   -- Const_Ref --
   ---------------

   function Const_Ref (This : Any; Pos : Any) return Const
   is (Element => References.Reference (This, Pos));

   ---------------
   -- Reference --
   ---------------

   function Reference (This : Any; Pos : Any) return Ref
   is (Element => References.Reference (This, Pos));

   ---------------
   -- Reference --
   ---------------

   function Reference (This : Any; Pos : UTF_8_String) return Ref
   is (This.Reference (Make.Str (Yeison_Utils.Decode (Pos))));

   ----------
   -- Self --
   ----------

   function Self (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

end Yeison_12;
