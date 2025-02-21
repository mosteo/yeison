package body Yeison_Generic.Operators is

   --  This whole package is an attempt at working around a bug in GNAT 10/11
   --  related to case records in nested generic packages. Or something.

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Client_Any) return Client_Any is
   begin
      if L.Kind in Scalar_Kinds then
         return Result : Client_Any := Empty_Vec do
            Result.Append (L);
            Result.Append (R);
         end return;
      elsif L.Kind in Vec_Kind then
         return Result : Client_Any := L do
            Result.Append (R);
         end return;
      else
         raise Constraint_Error with
           "Cannot append using ""/"" when left operator is: "
           & L.Kind'Image;
      end if;
   end "/";

   ----------
   -- Make --
   ----------

   package body Make is

      ---------
      -- Nil --
      ---------

      function Nil return Client_Any
      is (To_Any (Any (New_Nil)));

      ------------
      -- Scalar --
      ------------

      function Scalar (This : Yeison_Generic.Scalar) return Client_Any
      is
         Pre : constant Any'Class :=
                 (case This.Data.Kind is
                     when Bool_Kind => New_Bool (This.Data.Bool),
                     when Int_Kind  => New_Int  (This.Data.Int),
                     when Real_Kind => New_Real (This.Data.Real),
                     when Str_Kind  => New_Text (S (This.Data.Str)));
      begin
         return To_Any (Any (Pre));
      end Scalar;

      -----------
      -- False --
      -----------

      function False return Client_Any
      is (Make.Scalar (Scalars.New_Bool (False)));

      ----------
      -- True --
      ----------

      function True return Client_Any
      is (Make.Scalar (Scalars.New_Bool (True)));

      ----------
      -- Bool --
      ----------

      function Bool (This : Boolean) return Client_Any
      is (Make.Scalar (Scalars.New_Bool (This)));

      ---------
      -- Int --
      ---------

      function Int (This : Int_Type) return Client_Any
      is (Make.Scalar (Scalars.New_Int (This)));

      ----------
      -- Real --
      ----------

      function Real (This : Real_Type) return Client_Any
      is (Make.Scalar (Scalars.New_Real (This)));

      ---------
      -- Str --
      ---------

      function Str (This : Wide_Wide_String) return Client_Any
      is (Make.Scalar (Scalars.New_Text (This)));

   end Make;

   ---------
   -- Vec --
   ---------

   function Vec (This : Any_Array) return Client_Any is
   begin
      return Result : Client_Any := Empty_Vec do
         for Elem of This loop
            Result.Append (Elem);
         end loop;
      end return;
   end Vec;

end Yeison_Generic.Operators;
