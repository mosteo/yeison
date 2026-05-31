pragma Ada_2022;

procedure Yeison_Tests.Comparisons is
   S : Any renames Yeison_Tests.Sample;
begin
   --  Equality with literals on the right-hand side (no Text/Int overloads
   --  needed: the literal aspects make "one"/2 into Any).
   Assert (S ("one") = "one",        "any = string literal");
   Assert ("one" = S ("one"),        "string literal = any (symmetric)");
   Assert (S ("two") = 2,            "any = int literal");
   Assert (2 = S ("two"),           "int literal = any (symmetric)");
   Assert (S ("five") = 5.5,         "any = real literal");

   Assert (not (S ("one") = 2),      "different kinds are unequal");
   Assert (not (S ("one") = "two"),  "different strings unequal");

   --  Ordering: kinds order by their position, then by value
   Assert (Any'(1) < Any'(2),        "int ordering");
   Assert (Any'("a") < Any'("b"),    "string ordering");
   Assert (Any'(True) < Any'(1),     "bool kind precedes int kind");
   Assert (Precedes (Any'(1), Any'("z")), "int kind precedes str kind");

   --  Whole-structure equality
   Assert (Sample = Sample,          "structural equality");
end Yeison_Tests.Comparisons;
