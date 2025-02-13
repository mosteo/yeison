package Yeison_Multi with Preelaborate is

   type Any is tagged private;

   type Scalar is tagged private with
     Integer_Literal => To_Int,
     String_Literal => To_Str;

   function To_Int (Img : String) return Scalar;
   function To_Str (Img : Wide_Wide_String) return Scalar;

   type Map is tagged private with
     Aggregate => (Empty => Empty,
                   Add_Named => Insert);

   function Empty return Map;

   procedure Insert (This : in out Map; Key : String; Val : Any'Class);
   procedure Insert (This : in out Map; Key : String; Val : Scalar'Class);
   procedure Insert (This : in out Map; Key : String; Val : Map);

private

   type Any is tagged null record;

   type Scalar is tagged null record;

   type Map is tagged null record;

end Yeison_Multi;
