with Ada.Text_IO; use Ada.Text_IO;

with Yeison_Single;

procedure Demo_Single is

   package Yeison renames Yeison_Single;
   use Yeison.Operators;

   A1 : constant Yeison.Any := 1;
   --  An integer atom;

   A2 : constant Yeison.Any := "string";
   --  A string atom

   A3 : constant Yeison.Any := Yeison.True;
   --  A Boolean atom

   A4 : constant Yeison.Any := 3.14;
   --  A real atom

   M1 : constant Yeison.Any := ("one" => A1, "two" => A2);
   --  A map initialized with yeison atoms

   M2 : constant Yeison.Any := ("one" => 1, "two" => "two");
   --  A map initialized with literals

   M3 : constant Yeison.Any := ("one" => A1, "two" => "two", "three" => M2);
   --  A map containing other maps

   V1 : constant Yeison.Any := +(A1, A2);
   --  A vector initialized with atoms

   V2 : constant Yeison.Any := +(1, 2, 3) with Unreferenced;
   --  A vector initialized with integer literals

   V3 : constant Yeison.Any := +("one", "two", "three") with Unreferenced;
   --  A vector initialized with string literals

   V4 : constant Yeison.Any := ("one", 2, "three", 4.0);
   --  A vector made of mixed atoms/literals

   M4 : constant Yeison.Any := ("one" => A1, "two" => 2, "three" => M3, "four" => V4);
   --  A map initialized with all kinds of elements

   V5 : constant Yeison.Any := (A1, 2, M3, V4, "five");
   --  A vector initialized with all kinds of elements

   M5 : constant Yeison.Any := ("one" => 1,
                                "two" => ("two"   => 2,
                                          "three" => M3),
                                "zri" => +(1, 2, 3));
   --  Inline declaration of nested maps/vectors. Unfortunately the qualification is mandatory.

   V6 : constant Yeison.Any := (1,
                                +(1, 2),
                                ("one" => 1,
                                 "two" => M2));
   --  A vector with a nested vector/map. Same problem as with maps.

   X0 : Yeison.Any;
   X1 : constant Yeison.Any := 1;
   X2 : constant Yeison.Any := "two";
   X3 : constant Yeison.Any := M4;
   X4 : constant Yeison.Any := V5;
   --  Storing any kind of value in a variable

begin
   X0 := 1;
   Put_Line (X0.Image);
   X0 := "one";
   Put_Line (X0.Image);

   Put_Line (M1.Image);
   Put_Line (V1.Image);
   Put_Line (M5.Image);

   Put_Line (M4 ("one").Image);
   Put_Line (V6 (1).Image);
end Demo_Single;
