with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
--  with Ada.Tags;
with Ada.Unchecked_Deallocation;

--  with GNAT.IO; use GNAT.IO;

package body Yeison_Classwide is

   use Ada.Finalization;
   use Ada.Strings.Unbounded;

   ---------------
   -- Operators --
   ---------------

   package body Operators is

      function "+" (This : Vec) return Vec is
      begin
         return Result : constant Vec := This;
      end "+";

   end Operators;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (V : in out Abstract_Value) is
   begin
      if V.Concrete /= null then
         V.Concrete := new Abstract_Value'Class'(V.Concrete.all);
      end if;
   end Adjust;

   ------------
   -- As_Map --
   ------------

   function As_Map (This : aliased Abstract_Value'Class) return access constant Map'Class
   is (Map'Class (This)'Access);

   function As_Map (This : Abstract_Value'Class; Key : String)
                    return access constant Abstract_Value'Class
   is (Map'Class (This).Map_Constant_Reference (Key));

   ---------------
   -- As_String --
   ---------------

   function As_String (This : Abstract_Value'Class) return String
   is (if This.Concrete /= null
       then To_String (Str (This.Concrete.all).Value)
       else To_String (Str (This).Value));

   ------------
   -- As_Vec --
   ------------

   function As_Vec (This : aliased Abstract_Value'Class) return access constant Vec'Class
   is (Vec'Class (This)'Access);

   function As_Vec (This : Abstract_Value'Class; Index : Positive)
                    return access constant Abstract_Value'Class
   is (Vec'Class (This).Vec_Constant_Reference (Index));

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Map_Constant_Reference (This : Map'Class; Key : String)
                                    return access constant Abstract_Value'Class
   is (This.Value.Constant_Reference (Key).Element);

   function Map_Constant_Reference (This : Map'Class; Keys : Vec'Class)
                                    return access constant Abstract_Value'Class
   is
   begin
      if Keys.Length = 1 then
         return This (Keys.Value.First_Element.As_String);
      else
         declare
            Remaining_Keys : Vec'Class := Keys;
         begin
            Remaining_Keys.Value.Delete_First;
            return This.Map_Constant_Reference (Remaining_Keys);
         end;
      end if;
   end Map_Constant_Reference;

   function Vec_Constant_Reference (This : Vec'Class; Index : Positive)
                                    return access constant Abstract_Value'Class
   Is (This.Value.Constant_Reference (Index).Element);

   function Vec_Constant_Reference (This : Vec'Class; Indices : Multi_Dim_Index)
                                    return access constant Abstract_Value'Class
   is (if Indices'Length = 1
       then This (Indices (Indices'First))
       else Vec (This.Value.Constant_Reference (Indices (Indices'First)).Element.all)
              .Vec_Constant_Reference (Indices (Indices'First + 1 .. Indices'Last)));

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (V : in out Abstract_Value) is
      procedure Free is new Ada.Unchecked_Deallocation (Abstract_Value'Class, Ptr);
   begin
       Free (V.Concrete);
   end Finalize;

   -----------
   -- Image --
   -----------

   function Image (V : Abstract_Value) return String is
   begin
      if V.Concrete /= null then
         return V.Concrete.Image;
      else
         return "null";
      end if;
   end Image;

   overriding function Image (V : Bool) return String is
   begin
      if V.Value then
         return "True";
      else
         return "False";
      end if;
   end Image;

   overriding function Image (V : Int) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Ada.Numerics.Big_Numbers.Big_Integers.To_String (V.Value),
         Side => Ada.Strings.Both);
   end Image;

   overriding function Image (V : Real) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Ada.Numerics.Big_Numbers.Big_Reals.To_String (V.Value),
         Side => Ada.Strings.Both);
   end Image;

   overriding function Image (V : Str) return String is
   begin
      return """" &  To_String (V.Value) & """";
   end Image;

   overriding function Image (V : Map) return String is
      use Maps;
      Result : Unbounded_String := To_Unbounded_String ("(");
   begin
      for I in V.Value.Iterate loop
         Append (Result, """" & Key (I) & """" & " => " & Element (I).Image);
         if I /= V.Value.Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ")");

      return To_String (Result);
   end Image;

   overriding function Image (V : Vec) return String is
      use Vectors;
      Result : Unbounded_String := To_Unbounded_String ("(");
   begin
      for I in V.Value.Iterate loop
         Append (Result, Element (I).Image);
         if I /= V.Value.Last then
            Append (Result, ", ");
         else
            Append (Result, ")");
         end if;
      end loop;

      return To_String (Result);
   end Image;

   ------------
   -- Length --
   ------------

   function Length (This : Vec) return Positive is (Positive (This.Value.Length));

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Abstract_Value is
   begin
      return (Controlled with Concrete =>
                 new Int'(Controlled with
                  Concrete => null,
                  Value    => Ada.Numerics.Big_Numbers.Big_Integers.From_String (Img)));
   end To_Int;

   -------------
   -- To_Real --
   -------------

   function To_Real (Img : String) return Abstract_Value is
   begin
      return (Controlled with Concrete =>
                 new Real'(Controlled with
                  Concrete => null,
                  Value    => Ada.Numerics.Big_Numbers.Big_Reals.From_String (Img)));
   end To_Real;

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Abstract_Value is
   begin
      return (Controlled with Concrete =>
                 new Str'(Controlled with
                  Concrete => null,
                  Value    =>
                  To_Unbounded_String
                    (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Img))));
   end To_Str;

   ------------
   -- To_Int --
   ------------

   overriding function To_Int (S : String) return Int is
   begin
      return (Controlled with
              Concrete => null,
              Value => Ada.Numerics.Big_Numbers.Big_Integers.From_String (S));
   end To_Int;

   -------------
   -- To_Real --
   -------------

   overriding function To_Real (Img : String) return Real is
   begin
      return (Controlled with
              Concrete => null,
              Value => Ada.Numerics.Big_Numbers.Big_Reals.From_String (Img));
   end To_Real;

   ------------
   -- To_Str --
   ------------

   overriding function To_Str (Img : Wide_Wide_String) return Str is
   begin
      return (Controlled with
              Concrete => null,
              Value    =>
                To_Unbounded_String
                  (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Img)));
   end To_Str;

   -----------
   -- Empty --
   -----------

   function Empty return Map is
   begin
      return Map'(Controlled with Concrete => null, Value => Maps.Empty);
   end Empty;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This : in out Map; Key : String; Value : Abstract_Value'Class)
   is
   begin
      This.Value.Insert (Key, Value);
   end Insert;

   -----------
   -- Empty --
   -----------

   function Empty return Vec is
   begin
      return Vec'(Controlled with Concrete => null, Value => Vectors.Empty);
   end Empty;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Vec; Value : Abstract_Value'Class) is
   begin
      This.Value.Append (Value);
   end Append;

   ------------
   -- To_Int --
   ------------

   overriding function To_Int (S : String) return Vec is
   begin
      return Result : Vec do
         Result.Append (Int'(To_Int (S)));
      end return;
   end To_Int;

   overriding function To_Real (Img : String) return Vec is
   begin
      return Result : Vec do
         Result.Append (Real'(To_Real (Img)));
      end return;
   end To_Real;

   ------------
   -- To_Str --
   ------------

   overriding function To_Str (S : Wide_Wide_String) return Vec is
   begin
      return Result : Vec do
         Result.Append (Str'(To_Str (S)));
      end return;
   end To_Str;

end Yeison_Classwide;
