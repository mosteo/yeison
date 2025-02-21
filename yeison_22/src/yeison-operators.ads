with Yeison_Generic.Operators;

package Yeison.Operators with Preelaborate is

   package Impl is new Yeison.Impl.Operators (Any);

   function "+" (This : Impl.Any_Array) return Any renames Impl.Vec;

   function "/" (L, R : Any) return Any renames Impl."/";
   --  Vector concatenation a-la dir hierarchy

   package Make renames Impl.Make;

end Yeison.Operators;
