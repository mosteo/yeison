pragma Ada_2022;

package Yeison.Operators with Preelaborate is

   --  The literal and aggregate aspects cover scalars and maps; these two
   --  operators cover vectors (+[...]) and path-style building (a / b).

   function "+" (This : Any_Array) return Any with
     Post => "+"'Result.Kind = Vec_Kind;
   --  Build a vector from an array aggregate: +[1, "two", 3].

   function "/" (L, R : Any) return Any with
     Pre  => L.Kind in Scalar_Kinds | Vec_Kind,
     Post => "/"'Result.Kind = Vec_Kind;
   --  Append/build a vector: a / b / c. Also handy for nested indexing paths.

end Yeison.Operators;
