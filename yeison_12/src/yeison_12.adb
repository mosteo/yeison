package body Yeison_12 is

   ------------
   -- To_Any --
   ------------

   function To_Any (This : Impl.Any) return Any
   is (This with null record);

   package References is new Impl.References (Any, To_Any);

   ------------
   -- To_Vec --
   ------------

   function To_Vec (A : Any_Array) return Any is
   begin
      return Result : Any := Empty_Vec do
         for Elem of A loop
            Result.Append (Elem);
         end loop;
      end return;
   end To_Vec;

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

   ----------
   -- Self --
   ----------

   function Self (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

end Yeison_12;
