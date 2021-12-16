with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Unchecked_Deallocation;

package body Yeison is

   use Ada.Finalization;
   use Ada.Strings.Unbounded;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (V : in out Abstract_Value) is
   begin
      if V.Concrete /= null then
         V.Concrete := new Abstract_Value'Class'(V.Concrete.all);
      end if;
   end Adjust;

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

   overriding function Image (V : Int) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Ada.Numerics.Big_Numbers.Big_Integers.To_String (V.Value),
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
         Append (Result, Key (I) & " => " & Element (I).Image);
         if I /= V.Value.Last then
            Append (Result, ", ");
         else
            Append (Result, ")");
         end if;
      end loop;

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
   -- To_Int --
   ------------

   function To_Int (Img : String) return Abstract_Value is
   begin
      return (Controlled with Concrete =>
                 new Int'(Controlled with
                  Concrete => null,
                  Value    => Ada.Numerics.Big_Numbers.Big_Integers.From_String (Img)));
   end To_Int;

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

   ------------
   -- To_Int --
   ------------

   overriding function To_Int (S : String) return Map is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Int unimplemented");
      return raise Program_Error with "Unimplemented function To_Int";
   end To_Int;

   ------------
   -- To_Str --
   ------------

   overriding function To_Str (S : Wide_Wide_String) return Map is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Str unimplemented");
      return raise Program_Error with "Unimplemented function To_Str";
   end To_Str;

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
      pragma Compile_Time_Warning (Standard.True, "To_Int unimplemented");
      return raise Program_Error with "Unimplemented function To_Int";
   end To_Int;

   ------------
   -- To_Str --
   ------------

   overriding function To_Str (S : Wide_Wide_String) return Vec is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Str unimplemented");
      return raise Program_Error with "Unimplemented function To_Str";
   end To_Str;

end Yeison;
