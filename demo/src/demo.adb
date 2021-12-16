with Yeison;

with Ada.Text_IO; use Ada.Text_IO;

procedure Demo is

   A1 : Yeison.Int := 1;
   --  An integer atom;

   A2 : Yeison.Str := "string";
   --  A string atom

   M1 : Yeison.Map := ("one" => A1, "two" => A2);
   --  A map initialized with yeison atoms

   M2 : Yeison.Map := ("one" => 1, "two" => "two");
   --  A map initialized with literals

   M3 : Yeison.Map := ("one" => A1, "two" => "two", "three" => M2);
   --  A map containing other maps

   V1 : Yeison.Vec := (A1, A2);
   --  A vector initialized with atoms

   V2 : Yeison.Vec := (1, 2, 3);
   --  A vector initialized with integer literals

   V3 : Yeison.Vec := ("one", "two", "three");
   --  A vector initialized with string literals

   V4 : Yeison.Vec := ("one", 2, "three");
   --  A vector made of mixed atoms/literals

   M4 : Yeison.Map := ("one" => A1, "two" => 2, "three" => M3, "four" => V4);
   --  A map initialized with all kinds of elements

   V5 : Yeison.Vec := (A1, 2, M3, V4, "five");
   --  A vector initialized with all kinds of elements

   M5 : Yeison.Map := ("one" => 1,
                       "two" => Yeison.Map'("two"   => 2,
                                            "three" => M3),
                       "zri" => Yeison.Vec'(1, 2, 3));
   --  Inline declaration of nested maps/vectors. Unfortunately the qualification is mandatory.

   V6 : Yeison.Vec := (1,
                       Yeison.Vec'(1, 2),
                       Yeison.Map'("one" => 1,
                                   "two" => M2));
   --  A vector with a nested vector/map. Same problem as with maps.

   X0 : Yeison.Abstract_Value;

   X1 : Yeison.Any'Class := 1;
   X2 : Yeison.Any'Class := "two";
   X3 : Yeison.Any'Class := M4;
   X4 : Yeison.Any'Class := V5;
   --  Storing any kind of value in a variable

begin
   Put_Line ("X0: " & X0.Image);
   Put_Line ("X1: " & X1.Image);
   Put_Line ("X2: " & X2.Image);
   Put_Line ("X3: " & X3.Image);
   Put_Line ("X4: " & X4.Image);

   Put_Line ("M5: " & M5.Image);
   Put_Line ("V6: " & V6.Image);
end Demo;
