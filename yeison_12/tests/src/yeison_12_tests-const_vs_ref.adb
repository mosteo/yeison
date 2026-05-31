with Yeison_12; use Yeison_12;

--  Exercises the constant-view vs mutable-view semantics in every combination
--  of the API the author could think of. The two pillars are:
--
--    * Constant indexing (any read context) returns a read-only view of the
--      element and requires a valid index/key, raising Constraint_Error
--      otherwise. It never mutates the container. Copying that view into a
--      fresh Any yields an independent object (value semantics on assignment).
--
--    * Variable indexing (assignment target) returns a live, writable view,
--      auto-vivifies a nil into the proper container, grows a vector by exactly
--      one past its end, and creates a missing map key.
--
--  Whole-Any assignment is a deep, independent copy (value semantics).

procedure Yeison_12_Tests.Const_Vs_Ref is
   use Operators;
begin
   ---------------------------------------------------------------------------
   --  1. Whole-value assignment replaces in place, including a type change  --
   ---------------------------------------------------------------------------

   declare
      X : Any := +1;
   begin
      X := +"now a string";
      Assert (X.Kind = Str_Kind, "1: type change via assignment");
      Assert (X.As_Text = "now a string", "1: new value");
   end;

   ---------------------------------------------------------------------------
   --  2. Assignment is a deep, independent copy (value semantics)          --
   ---------------------------------------------------------------------------

   declare
      A : constant Any := To_Vec ((+1, +2, +3));
      B : Any := A;
   begin
      Assert (A = B, "2: a fresh copy compares equal");

      B (1) := +99;                      --  mutate the copy in place
      Assert (B (1).As_Int = 99, "2: copy was mutated");
      Assert (A (1).As_Int = 1, "2: original is untouched");
      Assert (A /= B, "2: copies are now independent");
   end;

   ---------------------------------------------------------------------------
   --  3. A copy taken from a CONSTANT object is itself mutable             --
   ---------------------------------------------------------------------------

   declare
      C : constant Any := To_Vec ((+10, +20));
      D : Any := C;
   begin
      D (2) := +21;
      Assert (D (2).As_Int = 21, "3: copy of a constant is mutable");
      Assert (C (2).As_Int = 20, "3: the constant source is unchanged");
   end;

   ---------------------------------------------------------------------------
   --  4. Reading is a snapshot: a copy does not alias the container        --
   ---------------------------------------------------------------------------

   declare
      M    : Any := Empty_Map.Insert (+"k", To_Vec ((+1, +2, +3)));
      Snap : constant Any := M ("k");    --  constant indexing -> a copy
   begin
      M ("k") (1) := +100;               --  variable indexing -> in place
      Assert (M ("k") (1).As_Int = 100, "4: in-place mutation is visible");
      Assert (Snap (1).As_Int = 1, "4: the earlier snapshot is frozen");
   end;

   ---------------------------------------------------------------------------
   --  5. Mutating a copy of a nested element does not touch the original   --
   ---------------------------------------------------------------------------

   declare
      M   : constant Any := Empty_Map.Insert (+"v", To_Vec ((+1, +2)));
      Sub : Any := M ("v");              --  copy of the inner vector
   begin
      Sub (1) := +42;
      Assert (Sub (1).As_Int = 42, "5: the copy changed");
      Assert (M ("v") (1).As_Int = 1, "5: the original element is intact");
   end;

   ---------------------------------------------------------------------------
   --  6. Get returns a copy, exactly like constant indexing               --
   ---------------------------------------------------------------------------

   declare
      M : constant Any := Empty_Map.Insert (+"n", +7);
      G : Any := M.Get (+"n");
   begin
      G := +999;
      Assert (G.As_Int = 999, "6: Get result is an independent copy");
      Assert (M ("n").As_Int = 7, "6: source untouched by mutating the copy");
   end;

   ---------------------------------------------------------------------------
   --  7. Same key, opposite behaviour: reading a missing key RAISES,      --
   --     writing a missing key CREATES it                                  --
   ---------------------------------------------------------------------------

   declare
      M      : Any := Empty_Map;
      Tmp    : Any with Unreferenced;
      Raised : Boolean := False;
   begin
      begin
         Tmp := M ("absent");            --  read -> constant indexing
      exception
         when Constraint_Error => Raised := True;
      end;
      Assert (Raised, "7: reading a missing key must raise");
      Assert (not M.Has_Key ("absent"), "7: a failed read must not create it");

      M ("absent") := +5;                --  write -> variable indexing
      Assert (M.Has_Key ("absent"), "7: writing a missing key creates it");
      Assert (M ("absent").As_Int = 5, "7: created with the written value");
   end;

   ---------------------------------------------------------------------------
   --  8. Vector bounds: read past end RAISES; write at length+1 GROWS;     --
   --     write beyond length+1 RAISES                                      --
   ---------------------------------------------------------------------------

   declare
      V      : Any := Empty_Vec.Append (+1).Append (+2).Append (+3);
      Tmp    : Any with Unreferenced;
      Raised : Boolean := False;
   begin
      --  Read past the end raises and leaves the vector alone
      begin
         Tmp := V (4);
      exception
         when Constraint_Error => Raised := True;
      end;
      Assert (Raised, "8: reading past the end must raise");
      Assert (V.Length = 3, "8: a failed read must not grow the vector");

      --  Write at length+1 appends
      V (4) := +4;
      Assert (V.Length = 4, "8: writing at length+1 grows the vector");
      Assert (V (4).As_Int = 4, "8: the new slot holds the written value");

      --  Write beyond length+1 raises
      Raised := False;
      begin
         V (6) := +6;
      exception
         when Constraint_Error => Raised := True;
      end;
      Assert (Raised, "8: writing beyond length+1 must raise");
      Assert (V.Length = 4, "8: a failed write must not grow the vector");
   end;

   ---------------------------------------------------------------------------
   --  9. Auto-vivification of a nil through variable indexing             --
   ---------------------------------------------------------------------------

   declare
      As_Map : Any;                      --  nil
      As_Vec : Any;                      --  nil
   begin
      As_Map ("k") := +1;                --  string key -> becomes a map
      Assert (As_Map.Kind = Map_Kind, "9: nil + string key -> map");

      As_Vec (1) := +1;                  --  integer index -> becomes a vector
      Assert (As_Vec.Kind = Vec_Kind, "9: nil + integer index -> vector");
   end;

   ---------------------------------------------------------------------------
   -- 10. Deep auto-vivification builds every missing intermediate level    --
   ---------------------------------------------------------------------------

   declare
      D : Any;                           --  nil
   begin
      D ("x") (1) ("y") := +5;
      Assert (D.Kind = Map_Kind, "10: root is a map");
      Assert (D ("x").Kind = Vec_Kind, "10: 'x' is a vector");
      Assert (D ("x") (1).Kind = Map_Kind, "10: 'x'(1) is a map");
      Assert (D ("x") (1) ("y").As_Int = 5, "10: leaf holds the value");
   end;

   ---------------------------------------------------------------------------
   -- 11. Reading a CONSTANT nested object works through constant indexing  --
   ---------------------------------------------------------------------------

   declare
      C : constant Any :=
        Empty_Map.Insert
          (+"a", Empty_Map.Insert
                   (+"b", To_Vec ((+"deep", +"er"))));
   begin
      Assert (C ("a") ("b") (1) = "deep", "11: nested constant read");
      Assert (C ("a") ("b") (2).As_Text = "er", "11: nested constant read 2");
   end;

   ---------------------------------------------------------------------------
   -- 12. Nested writes mutate in place and are independent of prior copies --
   ---------------------------------------------------------------------------

   declare
      Tree : Any :=
        Empty_Map.Insert (+"list", To_Vec ((+1, +2, +3)));
      Before : constant Any := Tree;     --  deep copy of the whole tree
   begin
      Tree ("list") (2) := +222;
      Tree ("extra") := +"new";

      Assert (Tree ("list") (2).As_Int = 222, "12: nested write visible");
      Assert (Tree.Has_Key ("extra"), "12: new key added");

      --  The pre-mutation deep copy is wholly unaffected
      Assert (Before ("list") (2).As_Int = 2, "12: prior copy frozen");
      Assert (not Before.Has_Key ("extra"), "12: prior copy has no new key");
   end;

   ---------------------------------------------------------------------------
   -- 13. Iterating yields read-only copies; the container is unchanged     --
   ---------------------------------------------------------------------------

   declare
      V    : constant Any := To_Vec ((+1, +2, +3));
      W    : Any := To_Vec ((+1, +2, +3));
      Sum  : Big_Int := 0;
      SumW : Big_Int := 0;
      SumT : Big_Int := 0;
   begin
      for E of V loop
         Sum := Sum + E.As_Int;
      end loop;
      for E of W loop
         SumW := SumW + E.As_Int;
      end loop;
      for E of To_Vec ((+1, +2, +3)) loop
         SumT := SumT + E.As_Int;
      end loop;
      Assert (Sum = 6,
              "13: const sum=" & Sum'Image & " var=" & SumW'Image
              & " temp=" & SumT'Image);
      Assert (V.Length = 3, "13: iteration did not alter the container");
   end;

end Yeison_12_Tests.Const_Vs_Ref;
