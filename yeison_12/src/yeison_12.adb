package body Yeison_12 is

   ------------
   -- To_Any --
   ------------

   function To_Any (This : Impl.Any) return Any
   is (This with null record);

   ----------------
   -- References --
   ----------------

   package References is new Impl.References (Any, To_Any);

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Any; Str : Text) is
   begin
      This.Append (Make.Str (Str));
   end Append;

   ------------
   -- Append --
   ------------

   function Append (This : Any; Str : Text) return Any
   is (This.Append (Make.Str (Str)));

   ----------
   -- Keys --
   ----------

   function Keys (This : Any; Ordered : Boolean := False) return Any
                  renames References.Keys;

   ---------------
   -- Iterators --
   ---------------

   package Iterators is new Impl.Iterators (Any, To_Any);

   -----------
   -- First --
   -----------

   function First (Container : Any) return Impl.Cursor renames
     Iterators.First_Cursor;

   ----------
   -- Next --
   ----------

   function Next (Container : Any; Position : Impl.Cursor) return Impl.Cursor renames
     Iterators.Next_Cursor;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Container : Any; Position : Impl.Cursor) return Boolean renames
     Iterators.Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Container : Any; Position : Impl.Cursor) return Any renames
     Iterators.Element;

   -------------
   -- Has_Key --
   -------------

   function Has_Key (This : Any; Key : UTF_8_String) return Boolean
   is (References.Has_Key (This, Make.Str (Yeison_Utils.Decode (Key))));

   ---------------
   -- Const_Ref --
   ---------------

   --  function Const_Ref (This : Any; Pos : Any) return Const
   --  is (Element => References.Reference (This, Pos));

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

   ---------------
   -- Reference --
   ---------------

   function Reference (This : Any; Pos : Big_Int) return Ref
   is (This.Reference (Make.Int (Pos)));

   ----------
   -- Self --
   ----------

   function Self (This : aliased Any) return Ref
   is (Element => This'Unrestricted_Access);

end Yeison_12;
