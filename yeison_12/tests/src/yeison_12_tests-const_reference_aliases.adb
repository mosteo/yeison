with Yeison_12; use Yeison_12;

--  Constant indexing now returns a read-only view (Const) of the designated
--  element rather than a by-value copy. The observable difference is that two
--  successive constant-index reads of the same position denote the SAME stored
--  object, whereas a by-value result would produce a distinct temporary each
--  time. Value semantics on assignment are unaffected: copying into a fresh Any
--  still yields an independent object (covered elsewhere).
--
--  Element access values are compared via the explicit Constant_Reference form
--  ("=" on the anonymous access-to-constant). The indexing-syntax form
--  (M ("k")) cannot be used as a 'Address prefix because it is overloaded with
--  variable indexing.

procedure Yeison_12_Tests.Const_Reference_Aliases is
   use Operators;
begin
   ---------------------------------------------------------------------------
   --  1. Map: repeated constant indexing aliases the same element          --
   ---------------------------------------------------------------------------

   declare
      M : constant Any := Empty_Map.Insert (+"k", +42);
   begin
      Assert (M.Constant_Reference (+"k").Element
                = M.Constant_Reference (+"k").Element,
              "1: repeated constant indexing must denote the same element");
      Assert (M ("k").As_Int = 42, "1: value read through the view");
   end;

   ---------------------------------------------------------------------------
   --  2. Vector: repeated constant indexing aliases the same element       --
   ---------------------------------------------------------------------------

   declare
      V : constant Any := To_Vec ((+10, +20, +30));
   begin
      Assert (V.Constant_Reference (Big_Int'(2)).Element
                = V.Constant_Reference (Big_Int'(2)).Element,
              "2: repeated constant indexing must denote the same element");
      Assert (V (2).As_Int = 20, "2: value read through the view");
   end;

   ---------------------------------------------------------------------------
   --  3. The constant view is live, not a frozen snapshot: it reflects an  --
   --     in-place mutation made through variable indexing afterwards       --
   ---------------------------------------------------------------------------

   declare
      M : Any := Empty_Map.Insert (+"k", +1);
   begin
      M ("k") := +7;                       --  variable indexing, in place
      Assert (M ("k").As_Int = 7,
              "3: constant view reflects the in-place update");
   end;

end Yeison_12_Tests.Const_Reference_Aliases;
