with Yeison_12; use Yeison_12;

procedure Yeison_12_Tests.Slash_Operator is
   use Operators;
begin
   --  Two scalars produce a Vec of length 2
   declare
      V : constant Any := (+1) / (+2);
   begin
      Assert (V.Kind = Vec_Kind, "scalar / scalar: kind is Vec");
      Assert (V.Length = 2,      "scalar / scalar: length 2");
      Assert (V (1).As_Int = 1,  "scalar / scalar: first element");
      Assert (V (2).As_Int = 2,  "scalar / scalar: second element");
   end;

   --  Chained: three scalars produce a Vec of length 3
   declare
      V : constant Any := ((+1) / (+2)) / (+3);
   begin
      Assert (V.Kind = Vec_Kind, "chained /: kind is Vec");
      Assert (V.Length = 3,      "chained /: length 3");
      Assert (V (1).As_Int = 1,  "chained /: element 1");
      Assert (V (2).As_Int = 2,  "chained /: element 2");
      Assert (V (3).As_Int = 3,  "chained /: element 3");
   end;

   --  Vec / scalar appends to the vec
   declare
      Base : constant Any := (+1) / (+2);
      V    : constant Any := Base / (+3);
   begin
      Assert (V.Kind = Vec_Kind, "vec / scalar: kind is Vec");
      Assert (V.Length = 3,      "vec / scalar: length 3");
      Assert (V (3).As_Int = 3,  "vec / scalar: appended element");
   end;

   --  Mixed types (string and int in same vec)
   declare
      V : constant Any := (+1) / (+"hello");
   begin
      Assert (V.Kind = Vec_Kind,          "mixed /: kind is Vec");
      Assert (V.Length = 2,               "mixed /: length 2");
      Assert (V (1).As_Int = 1,           "mixed /: int element");
      Assert (V (2).As_Text = "hello",    "mixed /: str element");
   end;
end Yeison_12_Tests.Slash_Operator;
