package body Yeison_12 is

   Unimplemented : exception;

   package References is new Impl.References (Any);

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

   function Const_Ref (This : aliased Any; Pos : Any) return Const
   is (raise Unimplemented);

   ---------------
   -- Reference --
   ---------------

   function Reference (This : aliased Any; Pos : Any) return Ref
   is (Element => References.Reference (This, Pos));

   ----------
   -- Self --
   ----------

   function Self (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

end Yeison_12;
