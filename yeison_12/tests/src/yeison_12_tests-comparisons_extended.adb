with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Comparisons_Extended is
   use Operators;

   -- Test maps
   Map1 : constant Any := Empty_Map.Insert (+"a", +1).Insert (+"b", +2);
   Map2 : constant Any := Empty_Map.Insert (+"a", +1).Insert (+"b", +3); -- Different value
   Map3 : constant Any := Empty_Map.Insert (+"a", +1).Insert (+"c", +2); -- Different key
   Map4 : constant Any := Empty_Map.Insert (+"a", +1); -- Shorter map

   -- Test vectors
   Vec1 : constant Any := Empty_Vec.Append (+1).Append (+2).Append (+3);
   Vec2 : constant Any := Empty_Vec.Append (+1).Append (+2).Append (+4); -- Different last element
   Vec3 : constant Any := Empty_Vec.Append (+1).Append (+3).Append (+3); -- Different middle element
   Vec4 : constant Any := Empty_Vec.Append (+1).Append (+2); -- Shorter vector
begin
   Assert (Yeison_12."<" (Map1, Map2));

   -- Test map comparisons
   Assert (not (Map1 < Map1), "Map should not be less than itself");
   Assert (Map1 < Map2, "Map1 should be less than Map2 (same keys, different values)");
   Assert (Map1 < Map3, "Map1 should be less than Map3 (different keys)");

   -- Test map length comparisons
   Assert (Map4 < Map1, "Shorter map should be less than longer map");
   Assert (not (Map1 < Map4), "Longer map should not be less than shorter map");

   -- Test vector comparisons
   Assert (not (Vec1 < Vec1), "Vector should not be less than itself");
   Assert (Vec1 < Vec2, "Vec1 should be less than Vec2 (different last element)");
   Assert (Vec1 < Vec3, "Vec1 should be less than Vec3 (different middle element)");

   -- Test vector length comparisons
   Assert (Vec4 < Vec1, "Shorter vector should be less than longer vector");
   Assert (not (Vec1 < Vec4), "Longer vector should not be less than shorter vector");

   -- Test mixed comparisons (different kinds)
   Assert (Map1 < Vec1, "Map should be less than Vector (different kinds)");
   Assert (not (Vec1 < Map1), "Vector should not be less than Map (different kinds)");
end Yeison_12_Tests.Comparisons_Extended;
