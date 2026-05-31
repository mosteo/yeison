pragma Ada_2022;

package Yeison.Operators with Preelaborate is

   --  The literal and aggregate aspects cover scalars and maps; these two
   --  operators cover vectors (+[...]) and path-style building (a / b).

   --  FUTURE (re-test when GNAT 16 is added): a single type defining BOTH
   --  Add_Unnamed and Add_Named in its Aggregate aspect (so [1, 2, 3] builds a
   --  vector and ["k" => v] a map) is still rejected by GNAT 14.2 and 15.2
   --  ("conflicting operations for aggregate (RM 4.3.5)"), even with -gnatX.
   --  Once GNAT implements that amendment, give Yeison.Any an
   --  Add_Unnamed => Append and drop this "+" operator: +[...] becomes [...].

   function "+" (This : Any_Array) return Any with
     Post => "+"'Result.Kind = Vec_Kind;
   --  Build a vector from an array aggregate: +[1, "two", 3].

   function "/" (L, R : Any) return Any with
     Pre  => L.Kind in Scalar_Kinds | Vec_Kind,
     Post => "/"'Result.Kind = Vec_Kind;
   --  Append/build a vector: a / b / c. Also handy for nested indexing paths.

end Yeison.Operators;
