with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Has_Value is
begin
   Assert (not Make.Nil.Has_Value,       "Nil: Has_Value = False");
   Assert (Make.True.Has_Value,          "Bool True: Has_Value = True");
   Assert (Make.False.Has_Value,         "Bool False: Has_Value = True");
   Assert (Make.Int (0).Has_Value,       "Int 0: Has_Value = True");
   Assert (Make.Int (-1).Has_Value,      "Int -1: Has_Value = True");
   Assert (Make.Real (Reals.New_Real (0.0)).Has_Value, "Real 0.0: Has_Value = True");
   Assert (Make.Str ("").Has_Value,      "Str empty: Has_Value = True");
   Assert (Make.Str ("x").Has_Value,     "Str non-empty: Has_Value = True");
   Assert (Empty_Map.Has_Value,          "Empty map: Has_Value = True");
   Assert (Empty_Vec.Has_Value,          "Empty vec: Has_Value = True");

   --  Has_Value is the complement of Is_Nil
   Assert (Make.Nil.Has_Value = not Make.Nil.Is_Nil,
           "Has_Value = not Is_Nil for Nil");
   Assert (Make.Int (1).Has_Value = not Make.Int (1).Is_Nil,
           "Has_Value = not Is_Nil for Int");
end Yeison_12_Tests.Has_Value;
