pragma Ada_2022;

procedure Yeison_Tests.Literals is
   B : constant Any := True;
   I : constant Any := 17;
   R : constant Any := 3.5;
   S : constant Any := "hello";
   N : constant Any := Nil;
begin
   Assert (B.Kind = Bool_Kind, "bool literal kind");
   Assert (B.Is_True,          "bool literal value");

   Assert (I.Kind = Int_Kind,  "int literal kind");
   Assert (I.As_Int = 17,      "int literal value");

   Assert (R.Kind = Real_Kind, "real literal kind");
   Assert (R.As_Real_Float = 3.5, "real literal value");

   Assert (S.Kind = Str_Kind,  "string literal kind");
   Assert (S.As_Text = "hello", "string literal value");

   Assert (N.Kind = Nil_Kind,  "nil kind");
   Assert (not N.Has_Value,    "nil has no value");

   --  Negative numeric literals (issue #6: unary "-" alongside To_Int/To_Real)
   declare
      NI : constant Any := -7;
      NR : constant Any := -3.5;
   begin
      Assert (NI.Kind = Int_Kind,        "negative int literal kind");
      Assert (NI.As_Int = -7,            "negative int literal value");
      Assert (NR.Kind = Real_Kind,       "negative real literal kind");
      Assert (NR.As_Real_Float = -3.5,   "negative real literal value");
   end;

   --  Negative literals via the strict subtypes
   declare
      SI : constant Yeison.Int  := -13;
      SR : constant Yeison.Real := -2.5;
   begin
      Assert (SI.As_Int = -13,           "negative Yeison.Int");
      Assert (SR.As_Real_Float = -2.5,   "negative Yeison.Real");
   end;

   --  Unary "-" applied to an Any value (double negation round-trips)
   Assert (As_Int (-I) = -17,            "unary minus on Any int");
   Assert (As_Int (- (-I)) = 17,         "double negation");
end Yeison_Tests.Literals;
