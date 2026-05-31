with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Any_Pos_Reference is
   use Operators;
begin
   --  Map indexed by an Any-typed string key
   declare
      M   : Any := Empty_Map;
      Key : constant Any := +"mykey";
   begin
      Reference (M, Key).Element.all := +42;
      Assert (M.Kind = Map_Kind,        "Any-key ref on map: kind");
      Assert (M ("mykey").As_Int = 42,  "Any-key ref on map: value");
   end;

   --  Vec indexed by an Any-typed integer key
   declare
      V   : Any := Empty_Vec.Append (+0).Append (+0).Append (+0);
      Idx : constant Any := +Big_Int'(2);
   begin
      Reference (V, Idx).Element.all := +99;
      Assert (V (2).As_Int = 99, "Any-key ref on vec: value at index 2");
      Assert (V (1).As_Int = 0,  "Any-key ref on vec: index 1 unchanged");
      Assert (V (3).As_Int = 0,  "Any-key ref on vec: index 3 unchanged");
   end;

   --  Auto-vivification from Nil via Any key (string => becomes a map)
   declare
      N   : Any := Make.Nil;
      Key : constant Any := +"x";
   begin
      Reference (N, Key).Element.all := +7;
      Assert (N.Kind = Map_Kind,    "Any-key ref auto-vivifies nil to map");
      Assert (N ("x").As_Int = 7,   "Auto-vivified map has correct value");
   end;

   --  Auto-vivification from Nil via Any integer key (=> becomes a vec)
   declare
      N   : Any := Make.Nil;
      Idx : constant Any := +Big_Int'(1);
   begin
      Reference (N, Idx).Element.all := +5;
      Assert (N.Kind = Vec_Kind,   "Any-key ref auto-vivifies nil to vec");
      Assert (N (1).As_Int = 5,    "Auto-vivified vec has correct value");
   end;
end Yeison_12_Tests.Any_Pos_Reference;
