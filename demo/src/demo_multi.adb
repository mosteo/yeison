with Yeison_Multi;

with Ada.Text_IO; use Ada.Text_IO;

with Alire_Crate;

procedure Demo_Multi is

   package Yeison renames Yeison_Multi;

   A1 : constant Yeison.Scalar := 1;
   --  An integer atom;

   A2 : constant Yeison.Scalar := "string";
   --  A string atom
   --
   --  A3 : constant Yeison.Bool := Yeison.True;
   --  --  A Boolean atom
   --
   --  A4 : constant Yeison.Real := 3.14;
   --  A real atom

   M1 : constant Yeison.Map := ("one" => A1, "two" => A2) with Unreferenced;
   --  A map initialized with yeison atoms

   --  M2 : constant Yeison.Map := ("one" => 1, "two" => "two");
   --  --  A map initialized with literals
   --
   --  M3 : constant Yeison.Map := ("one" => A1, "two" => "two", "three" => M2);
   --  --  A map containing other maps
   --
   --  V1 : constant Yeison.Vec := (A1, A2) with Unreferenced;
   --  --  A vector initialized with atoms
   --
   --  V2 : constant Yeison.Vec := (1, 2, 3) with Unreferenced;
   --  --  A vector initialized with integer literals
   --
   --  V3 : constant Yeison.Vec := ("one", "two", "three") with Unreferenced;
   --  --  A vector initialized with string literals
   --
   --  V4 : constant Yeison.Vec := ("one", 2, "three", 4.0);
   --  --  A vector made of mixed atoms/literals
   --
   --  M4 : constant Yeison.Map := ("one" => A1, "two" => 2, "three" => M3, "four" => V4);
   --  --  A map initialized with all kinds of elements
   --
   --  V5 : constant Yeison.Vec := (A1, 2, M3, V4, "five");
   --  --  A vector initialized with all kinds of elements
   --
   --  M5 : constant Yeison.Map := ("one" => 1,
   --                               "two" => Yeison.Map'("two"   => 2,
   --                                                    "three" => M3),
   --                               "zri" => Yeison.Vec'(1, 2, 3));
   --  --  Inline declaration of nested maps/vectors. Unfortunately the qualification is mandatory.
   --
   --  M6 : constant Yeison.Map := ("one" => 1,
   --                               "two" => Yeison.Map'("two"   => 2,
   --                                                    "three" => M3),
   --                               "zri" => Yeison.Vec'(1, M2, 3));
   --  --  Inline declaration of nested maps/vectors. Unfortunately the qualification is mandatory.
   --
   --  V6 : constant Yeison.Vec := (1,
   --                      Yeison.Vec'(1, 2),
   --                      Yeison.Map'("one" => 1,
   --                                  "two" => M2));
   --  --  A vector with a nested vector/map. Same problem as with maps.
   --
   --  X0 : Yeison.Any;
   --
   --  X1 : constant Yeison.Any'Class := 1;
   --  X2 : constant Yeison.Any'Class := "two";
   --  X3 : constant Yeison.Any'Class := M4;
   --  X4 : constant Yeison.Any'Class := V5;
   --  Storing any kind of value in a variable

begin
   null;
   --  Put_Line ("X0: " & X0.Image);
   --  X0 := "changed";
   --  Put_Line ("X0: " & X0.Image);
   --  X0 := 1;
   --  Put_Line ("X0: " & X0.Image);
   --
   --  Put_Line ("X1: " & X1.Image);
   --  Put_Line ("X2: " & X2.Image);
   --  Put_Line ("X3: " & X3.Image);
   --  Put_Line ("X4: " & X4.Image);
   --
   --  Put_Line ("M5: " & M5.Image);
   --  Put_Line ("V6: " & V6.Image);
   --
   --  Put_Line ("Crate: " & Alire_Crate.Crate.Image);
   --
   --  Put_Line ("Map indexing: M4 (""one"") => " & M4 ("one").Image);
   --  Put_Line ("Map nested indexing: Crate (""depends-on"") (""aaa"") => "
   --            & Alire_Crate.Crate ("depends-on").As_Map ("aaa").Image);
   --
   --  Put_Line ("Map nested indexing alt syntax: "
   --            & M5 (Yeison.Vec'("two", "two")).Image);
   --  Put_Line ("Map nested indexing alt syntax '+': "
   --            & M5 (+("two", "two")).Image);
   --  Put_Line ("Map nested indexing alt alt syntax: " & M5 ("two" / "two").Image);
   --
   --  Put_Line ("Map nested mixed indexing: " & M6 (Yeison.Vec'("zri", 2)).Image);
   --  Put_Line ("Nested mixed indexing alt: " & M6 ("zri" / 2 / "one").Image);
   --
   --  Put_Line ("Vec indexing: V6 (1) = " & V6 (1).Image);
   --  Put_Line ("Vec nested indexing V6 (2) (2) = "
   --            & V6 (2).As_Vec (2).Image);
   --  Put_Line ("Vec nested indexing alt syntax V6 ((2, 2)) = " & V6 ((2, 2)).Image);
   --  --  Put_Line ("Vec mixed indexing: " & V6 (3 / "one").Image);
   --
   --  Put_Line ("Real image: " & A4.Image);
end Demo_Multi;
