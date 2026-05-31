procedure Yeison_12_Tests.Negation is
   I    : constant Any := +3;
   R    : constant Any := +2.5;
   NegI : constant Any := +(-5);
begin
   --  Integer negation
   Assert ((-I) = +(-3),     "negate positive int");
   Assert ((-(-I)) = I,      "double negation int");
   Assert ((-NegI) = +5,     "negate negative int");

   --  Real negation
   Assert ((-R) = +(-2.5),   "negate positive real");
   Assert ((-(-R)) = R,      "double negation real");

   --  Kind is preserved
   Assert (Kind (-I) = Int_Kind,  "negated int is int");
   Assert (Kind (-R) = Real_Kind, "negated real is real");
end Yeison_12_Tests.Negation;
