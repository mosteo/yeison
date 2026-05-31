pragma Ada_2022;

procedure Yeison_Tests.Literals is
   B : constant Any := True;
   I : constant Any := 42;
   R : constant Any := 3.5;
   S : constant Any := "hello";
   N : constant Any := Nil;
begin
   Assert (B.Kind = Bool_Kind, "bool literal kind");
   Assert (B.Is_True,          "bool literal value");

   Assert (I.Kind = Int_Kind,  "int literal kind");
   Assert (I.As_Int = 42,      "int literal value");

   Assert (R.Kind = Real_Kind, "real literal kind");
   Assert (R.As_Real_Float = 3.5, "real literal value");

   Assert (S.Kind = Str_Kind,  "string literal kind");
   Assert (S.As_Text = "hello", "string literal value");

   Assert (N.Kind = Nil_Kind,  "nil kind");
   Assert (not N.Has_Value,    "nil has no value");
end Yeison_Tests.Literals;
