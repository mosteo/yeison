pragma Ada_2022;

package body Yeison.Operators is

   ---------
   -- "+" --
   ---------

   function "+" (This : Any_Array) return Any is
   begin
      return Result : Any := Empty_Vec do
         for Elem of This loop
            Result.Append (Elem);
         end loop;
      end return;
   end "+";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Any) return Any is
   begin
      if L.Kind in Scalar_Kinds then
         return Result : Any := Empty_Vec do
            Result.Append (L);
            Result.Append (R);
         end return;
      elsif L.Kind in Vec_Kind then
         return Result : Any := L do
            Result.Append (R);
         end return;
      else
         raise Constraint_Error with
           "Cannot append using ""/"" when left operator is: " & L.Kind'Image;
      end if;
   end "/";

end Yeison.Operators;
