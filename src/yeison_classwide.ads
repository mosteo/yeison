private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Ada.Numerics.Big_Numbers.Big_Integers;
private with Ada.Numerics.Big_Numbers.Big_Reals;

package Yeison_Classwide with Preelaborate is

   type Abstract_Value is tagged private with
     Integer_Literal => To_Int,
     Real_Literal    => To_Real,
     String_Literal  => To_Str;

   type Vec;

   function As_String (This : Abstract_Value'Class) return String;

   type Map;

   function As_Map (This : aliased Abstract_Value'Class) return access constant Map'Class;
   function As_Map (This : Abstract_Value'Class; Key : String)
                    return access constant Abstract_Value'Class;

   function As_Vec (This : aliased Abstract_Value'Class) return access constant Vec'Class;
   function As_Vec (This : Abstract_Value'Class; Index : Positive)
                    return access constant Abstract_Value'Class;

   function Image (V : Abstract_Value) return String;

   subtype Any is Abstract_Value'Class;

   function To_Int (Img : String) return Abstract_Value;
   function To_Real (Img : String) return Abstract_Value;
   function To_Str (Img : Wide_Wide_String) return Abstract_Value;

   type Bool is new Abstract_Value with private;

   function False return Bool;
   function True return Bool;

   overriding function Image (V : Bool) return String;

   type Int is new Abstract_Value with private with
     Integer_Literal => To_Int;

   overriding function Image (V : Int) return String;

   overriding function To_Int (S : String) return Int;

   type Real is new Abstract_Value with private
     with Real_Literal => To_Real;

   overriding function Image (V : Real) return String;

   overriding function To_Real (Img : String) return Real;

   type Str is new Abstract_Value with private
     with String_Literal => To_Str;

   overriding function Image (V : Str) return String;

   overriding function To_Str (Img : Wide_Wide_String) return Str;

   type Map is new Abstract_Value with private with
     Constant_Indexing       => Map_Constant_Reference,
     Aggregate => (Empty     => Empty,
                   Add_Named => Insert);

   function Empty return Map;

   overriding function Image (V : Map) return String;

   function Map_Constant_Reference (This : Map'Class; Key : String)
                                    return access constant Abstract_Value'Class;

   function Map_Constant_Reference (This : Map'Class; Keys : Vec'Class)
                                    return access constant Abstract_Value'Class;

   procedure Insert (This  : in out Map;
                     Key   : String;
                     Value : Abstract_Value'Class);

   overriding function To_Int (S : String) return Map;
   overriding function To_Real (Img : String) return Map;
   overriding function To_Str (S : Wide_Wide_String) return Map;

   type Vec is new Abstract_Value with private with
     Constant_Indexing => Vec_Constant_Reference,
     Aggregate => (Empty          => Empty,
                   Add_Unnamed    => Append);

   function Empty return Vec;

   function Length (This : Vec) return Positive;

   overriding function Image (V : Vec) return String;

   procedure Append (This : in out Vec; Value : Abstract_Value'Class);

   function Vec_Constant_Reference (This : Vec'Class; Index : Positive)
                                    return access constant Abstract_Value'Class;

   type Multi_Dim_Index is array (Positive range <>) of Positive;

   function Vec_Constant_Reference (This : Vec'Class; Indices : Multi_Dim_Index)
                                    return access constant Abstract_Value'Class;

   overriding function To_Int (S : String) return Vec;
   overriding function To_Real (Img : String) return Vec;
   overriding function To_Str (S : Wide_Wide_String) return Vec;

   package Operators is

      function "+" (This : Vec) return Vec with Inline_Always;

   end Operators;

private

   type Ptr is access all Abstract_Value'Class;

   type Abstract_Value is new Ada.Finalization.Controlled with record
      Concrete : Ptr;
   end record;

   overriding procedure Adjust (V : in out Abstract_Value);
   overriding procedure Finalize (V : in out Abstract_Value);

   type Bool is new Abstract_Value with record
      Value : Boolean;
   end record;

   overriding function To_Int (Img : String) return Bool is (raise Constraint_Error);
   overriding function To_Real (Img : String) return Bool is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Bool is (raise Constraint_Error);

   function False return Bool is (Abstract_Value with Value => Standard.False);
   function True  return Bool is (Abstract_Value with Value => Standard.True);

   type Int is new Abstract_Value with record
      Value : Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
   end record;

   overriding function To_Real (Img : String) return Int is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Int is (raise Constraint_Error);

   type Real is new Abstract_Value with record
      Value : Ada.Numerics.Big_Numbers.Big_Reals.Big_Real;
   end record;

   overriding function To_Int (Img : String) return Real is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Real is (raise Constraint_Error);

   type Str is new Abstract_Value with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function To_Int (S : String) return Str is (raise Constraint_Error);
   overriding function To_Real (Img : String) return Str is (raise Constraint_Error);

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, Abstract_Value'Class);

   type Map is new Abstract_Value with record
      Value : Maps.map;
   end record;

   overriding function To_Int (S : String) return Map is (raise Constraint_Error);
   overriding function To_Real (Img : String) return Map is (raise Constraint_Error);
   overriding function To_Str (S : Wide_Wide_String) return Map is (raise Constraint_Error);

   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Abstract_Value'Class);

   type Vec is new Abstract_Value with record
      Value : Vectors.Vector;
   end record;

end Yeison_Classwide;
