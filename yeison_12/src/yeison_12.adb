package body Yeison_12 is

   ---------
   -- Vec --
   ---------

   function Vec (A : Any_Array) return Any is
   begin
      return Result : Any := Empty_Vec do
         for Elem of A loop
            Result.Append (Elem);
         end loop;
      end return;
   end Vec;

end Yeison_12;
