with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Precedes is
   use Operators;
   A : constant Any := +1;
   B : constant Any := +2;
   S : constant Any := +"alpha";
   T : constant Any := +"beta";
begin
   Assert (Precedes (A, B),         "1 precedes 2");
   Assert (not Precedes (B, A),     "2 does not precede 1");
   Assert (not Precedes (A, A),     "x does not precede itself");
   Assert (Precedes (S, T),         """alpha"" precedes ""beta""");
   Assert (not Precedes (T, S),     """beta"" does not precede ""alpha""");

   --  Precedes is identical to "<"
   Assert (Precedes (A, B) = (A < B), "Precedes equals < for ints");
   Assert (Precedes (S, T) = (S < T), "Precedes equals < for strings");
end Yeison_12_Tests.Precedes;
