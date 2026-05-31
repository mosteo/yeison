with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Scalar_Type is
   use type Reals.General_Real;
begin
   declare
      S : constant Scalar := Scalars.New_Bool (True);
   begin
      Assert (S.Kind = Bool_Kind, "New_Bool True: kind");
      Assert (S.As_Boolean = True, "New_Bool True: value");
   end;

   declare
      S : constant Scalar := Scalars.New_Bool (False);
   begin
      Assert (S.Kind = Bool_Kind, "New_Bool False: kind");
      Assert (S.As_Boolean = False, "New_Bool False: value");
   end;

   declare
      S : constant Scalar := Scalars.New_Int (42);
   begin
      Assert (S.Kind = Int_Kind, "New_Int: kind");
      Assert (S.As_Integer = 42, "New_Int: value");
   end;

   declare
      S : constant Scalar := Scalars.New_Real (Reals.New_Real (3.14));
   begin
      Assert (S.Kind = Real_Kind, "New_Real: kind");
      Assert (S.As_Real = Reals.New_Real (3.14), "New_Real: value");
   end;

   declare
      S : constant Scalar := Scalars.New_Text ("hello");
   begin
      Assert (S.Kind = Str_Kind, "New_Text: kind");
      Assert (S.As_Text = "hello", "New_Text: value");
   end;

   --  As_Scalar extracts a Scalar from an Any
   declare
      A : constant Any    := Make.Bool (True);
      S : constant Scalar := A.As_Scalar;
   begin
      Assert (S.Kind = Bool_Kind, "As_Scalar of Bool Any: kind");
      Assert (S.As_Boolean = True, "As_Scalar of Bool Any: value");
   end;

   --  Get (Any'Class) return Scalar renaming
   declare
      A : constant Any    := Make.Int (99);
      S : constant Scalar := Get (A);
   begin
      Assert (S.Kind = Int_Kind, "Get as Scalar of Int Any: kind");
      Assert (S.As_Integer = 99, "Get as Scalar of Int Any: value");
   end;

   --  Make.Scalar round-trip: Scalar -> Any
   declare
      S : constant Scalar := Scalars.New_Text ("world");
      A : constant Any    := Make.Scalar (S);
   begin
      Assert (A.Kind = Str_Kind, "Make.Scalar of Str scalar: kind");
      Assert (A.As_Text = "world", "Make.Scalar of Str scalar: value");
   end;

   --  Make.Scalar round-trip: Int
   declare
      S : constant Scalar := Scalars.New_Int (-7);
      A : constant Any    := Make.Scalar (S);
   begin
      Assert (A.Kind = Int_Kind, "Make.Scalar of Int scalar: kind");
      Assert (A.As_Int = -7, "Make.Scalar of Int scalar: value");
   end;

   --  Make.Map and Make.Vec factories
   declare
      M : constant Any := Make.Map;
      V : constant Any := Make.Vec;
   begin
      Assert (M.Kind = Map_Kind, "Make.Map: kind");
      Assert (M.Is_Empty,        "Make.Map: empty");
      Assert (V.Kind = Vec_Kind, "Make.Vec: kind");
      Assert (V.Is_Empty,        "Make.Vec: empty");
   end;
end Yeison_12_Tests.Scalar_Type;
