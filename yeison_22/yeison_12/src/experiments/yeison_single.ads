private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;

package Yeison_Single with Preelaborate is

   subtype Text is String;

   type Any is tagged private with
     Integer_Literal   => To_Int,
     Real_Literal      => To_Real,
     String_Literal    => To_String,
     Constant_Indexing => Constant_Reference,
     Aggregate => (Empty     => Empty,
                   Add_Named => Insert);

   function Image (This : Any) return Text;

   function To_Int (Img : String) return Any;

   function To_Real (Img : String) return Any;

   function To_String (Img : Wide_Wide_String) return Any;

   function Constant_Reference (This : Any; Pos : Positive) return access constant Any;

   function Constant_Reference (This : Any; Key : String) return access constant Any;

   function Empty return Any;

   procedure Insert (This : in out Any; Key : Text; Val : Any);


   function True return Any;

   function False return Any;


   type Vec_Aux is private with
     Aggregate => (Empty => Empty,
                   Add_Unnamed => Append);

   function Empty return Vec_Aux;

   procedure Append (This : in out Vec_Aux; Val : Any);

   package Operators is

      function "+" (This : Vec_Aux) return Any;

   end Operators;

private

   type Inner_Any is interface;

   function Image (This : Inner_Any) return Text is abstract;

   type Inner_Bool is new Inner_Any with record
      Value : Boolean;
   end record;

   overriding function Image (This : Inner_Bool) return Text
   is (This.Value'Image);

   type Inner_Int is new Inner_Any with record
      Value : Integer;
   end record;

   overriding function Image (This : Inner_Int) return Text
   is (This.Value'Image);

   type Inner_Real is new Inner_Any with record
      Value : Float;
   end record;

   overriding function Image (This : Inner_Real) return Text
   is (This.Value'Image);

   type Inner_Str is new Inner_Any with record
      Value : access Text;
   end record;

   overriding function Image (This : Inner_Str) return Text
   is (This.Value.all);

   package Inner_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Inner_Any'Class);

   type Inner_Map is new Inner_Any with record
      Value : Inner_Maps.Map;
   end record;

   overriding function Image (This : Inner_Map) return Text;

   package Inner_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Inner_Any'Class);

   type Inner_Vec is new Inner_Any with record
      Value : Inner_Vectors.Vector;
   end record;

   overriding function Image (This : Inner_Vec) return Text;

   package Inner_Holders is new Ada.Containers.Indefinite_Holders (Inner_Any'Class);

   type Any is new Inner_Holders.Holder with null record;

   function Image (This : Any) return Text
   is (This.Element.Image);

   type Vec_Aux is record
      Value : Inner_Vec;
   end record;

end Yeison_Single;
