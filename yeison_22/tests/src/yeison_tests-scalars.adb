pragma Ada_2022;

procedure Yeison_Tests.Scalars is
   B : constant Any := True;
   I : constant Any := 7;
   R : constant Any := 2.5;
   S : constant Any := "text";
begin
   --  Direct accessors
   Assert (B.As_Bool = Standard.True, "As_Bool");
   Assert (I.As_Int = 7,              "As_Int");
   Assert (R.As_Real_Float = 2.5,     "As_Real_Float");
   Assert (S.As_Text = "text",        "As_Text");
   Assert (S.As_UTF_8 = "text",       "As_UTF_8");

   --  Get renamings (resolved by result type)
   declare
      GI : constant Big_Int := I.Get;
      GT : constant Text    := S.Get;
   begin
      Assert (GI = 7,        "Get -> Big_Int");
      Assert (GT = "text",   "Get -> Text");
   end;

   --  The separate Scalar view
   declare
      Sc : constant Scalar := I.As_Scalar;
   begin
      Assert (Sc.Kind = Int_Kind,     "scalar kind");
      Assert (Sc.As_Integer = 7,      "scalar integer");
   end;
end Yeison_Tests.Scalars;
