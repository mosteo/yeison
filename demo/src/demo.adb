with Yeison;

with Ada.Containers.Vectors;

procedure Demo is

   Int : Yeison.Collection := 3;
   -- A single integer value

   Str : Yeison.Collection := "Yeison";
   -- A single string value

   V1  : Yeison.Collection := (Int, Str);
   -- An array made of collections

   V2  : Yeison.Collection := (1, 2, 3);
   -- An array made of literals turned into collections

   M1  : Yeison.Collection := ("key" => Int);

begin
   null;
end Demo;
