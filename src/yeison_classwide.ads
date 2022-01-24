private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Ada.Numerics.Big_Numbers.Big_Integers;
private with Ada.Numerics.Big_Numbers.Big_Reals;

package Yeison_Classwide with Preelaborate is

   type Any is tagged private with
     Constant_Indexing => Constant_Reference,
     Integer_Literal => To_Int,
     Real_Literal    => To_Real,
     String_Literal  => To_Str;

   function Constant_Reference (This : Any'Class; Pos : Positive)
                                return access constant Any'Class;

   function Constant_Reference (This : Any'Class; Key : String)
                                return access constant Any'Class;

   type Vec is tagged;

   function Constant_Reference (This : Any'Class; Path : Vec'Class)
                                return not null access constant Any'Class;

   function Is_Scalar (This : Any'Class) return Boolean;

   function Tag (This : Any'Class) return String;

   function As_Integer (This : Any'Class) return Integer;
   function As_String (This : Any'Class) return String;

   type Map;

   function As_Map (This : Any'Class) return access constant Map'Class;
   function As_Map (This : Any'Class; Key : String)
                    return access constant Any'Class;

   function As_Vec (This : Any'Class) return access constant Vec'Class;
   function As_Vec (This : Any'Class; Index : Positive)
                    return access constant Any'Class;

   function Image (V : Any) return String;

   function To_Int (Img : String) return Any;
   function To_Real (Img : String) return Any;
   function To_Str (Img : Wide_Wide_String) return Any;

   type Bool is new Any with private;

   function False return Bool;
   function True return Bool;

   overriding function Image (V : Bool) return String;

   type Int is new Any with private with
     Integer_Literal => To_Int;

   overriding function Image (V : Int) return String;

   overriding function To_Int (S : String) return Int;

   type Real is new Any with private
     with Real_Literal => To_Real;

   overriding function Image (V : Real) return String;

   overriding function To_Real (Img : String) return Real;

   type Str is new Any with private
     with String_Literal => To_Str;

   overriding function Image (V : Str) return String;

   overriding function To_Str (Img : Wide_Wide_String) return Str;

   subtype Any_Scalar is Any'Class with
     Dynamic_Predicate => Any_Scalar in Bool | Int | Real | Str;

   type Map is new Any with private with
     Aggregate => (Empty     => Empty,
                   Add_Named => Insert);

   function Empty return Map;

   overriding function Image (V : Map) return String;

   procedure Insert (This  : in out Map;
                     Key   : String;
                     Value : Any'Class);

   overriding function To_Int (S : String) return Map;
   overriding function To_Real (Img : String) return Map;
   overriding function To_Str (S : Wide_Wide_String) return Map;

   type Vec is new Any with private with
     Aggregate => (Empty          => Empty,
                   Add_Unnamed    => Append);

   function Empty return Vec;

   function Length (This : Vec) return Positive;

   overriding function Image (V : Vec) return String;

   procedure Append (This : in out Vec; Value : Any'Class);

   function Get (This : Vec; Indices : Vec'Class)
                 return not null access constant Any'Class;

   overriding function To_Int (S : String) return Vec;
   overriding function To_Real (Img : String) return Vec;
   overriding function To_Str (S : Wide_Wide_String) return Vec;

   subtype Any_Composite is Any'Class with
     Dynamic_Predicate => Any_Composite in Map'Class | Vec'Class;

   package Operators is

      function "+" (This : Vec) return Vec with Inline_Always;

      function "/" (L : Any'Class; R : Any'Class) return Vec;

   end Operators;

private

   type Multi_Keys is limited null record;

   type Ptr is access all Any'Class;

   type Any is new Ada.Finalization.Controlled with record
      Concrete : Ptr;
   end record;

   function Is_Int (This : Any'Class) return Boolean
   is (This in Int or else (This.Concrete /= null and then This.Concrete.all in Int));

   function Is_Map (This : Any'Class) return Boolean
   is (This in Map or else (This.Concrete /= null and then This.Concrete.all in Map));

   function Is_Str (This : Any'Class) return Boolean
   is (This in Str or else (This.Concrete /= null and then This.Concrete.all in Str));

   function Is_Vec (This : Any'Class) return Boolean
   is (This in Vec or else (This.Concrete /= null and then This.Concrete.all in Vec));

   overriding procedure Adjust (V : in out Any);
   overriding procedure Finalize (V : in out Any);

   type Bool is new Any with record
      Value : Boolean;
   end record;

   overriding function To_Int (Img : String) return Bool is (raise Constraint_Error);
   overriding function To_Real (Img : String) return Bool is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Bool is (raise Constraint_Error);

   function False return Bool is (Any with Value => Standard.False);
   function True  return Bool is (Any with Value => Standard.True);

   type Int is new Any with record
      Value : Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
   end record;

   overriding function To_Real (Img : String) return Int is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Int is (raise Constraint_Error);

   type Real is new Any with record
      Value : Ada.Numerics.Big_Numbers.Big_Reals.Big_Real;
   end record;

   overriding function To_Int (Img : String) return Real is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Real is (raise Constraint_Error);

   type Str is new Any with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function To_Int (S : String) return Str is (raise Constraint_Error);
   overriding function To_Real (Img : String) return Str is (raise Constraint_Error);

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, Any'Class);

   type Map is new Any with record
      Value : Maps.Map;
   end record;

   overriding function To_Int (S : String) return Map is (raise Constraint_Error);
   overriding function To_Real (Img : String) return Map is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Map is (raise Constraint_Error);

   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Any'Class);

   type Vec is new Any with record
      Value : Vectors.Vector;
   end record;

end Yeison_Classwide;
