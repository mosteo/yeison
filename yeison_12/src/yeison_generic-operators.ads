---------------
-- Operators --
---------------

generic
   type Client_Any is new Yeison_Generic.Any with private;
   with function To_Any (This : Yeison_Generic.Any) return Client_Any is <>;
package Yeison_Generic.Operators with Preelaborate is

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Client_Any) return Client_Any with
     Pre  => L.Kind in Scalar_Kinds | Vec_Kind,
     Post => "/"'Result.Kind = Vec_Kind;

   --  Temporary workaround until both Add_Named and Add_Unnamed can be used
   --  simultaneously on the same type. It's convenient having it here so
   --  "+" becomes visible with the rest.

   type Any_Array is array (Positive range <>) of Client_Any;

   function Vec (This : Any_Array) return Client_Any with
     Post => Vec'Result.Kind = Vec_Kind;

   ----------
   -- Make --
   ----------

   package Make is
      function True  return Client_Any;
      function False return Client_Any;

      function Bool (This : Boolean)   return Client_Any;
      function Int  (This : Int_Type)  return Client_Any;
      function Real (This : Real_Type) return Client_Any;
      function Str  (This : Text)      return Client_Any;

      function Scalar (This : Yeison_Generic.Scalar) return Client_Any;
   end Make;

end Yeison_Generic.Operators;
