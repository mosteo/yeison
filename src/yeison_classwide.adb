with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;

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

      ---------
      -- "/" --
      ---------

      function "/" (L : Any'Class; R : Any'Class) return Vec is
      begin
         if not R.Is_Scalar then
            raise Constraint_Error with
              "Only scalars are intended for indexing in paths built with '/'";
         end if;

         if L in Vec then
            return Result : Vec := Vec (L) do
               Result.Append (R);
            end return;
         elsif L.Is_Scalar then
            return Result : Vec do
               Result.Append (L);
               Result.Append (R);
            end return;
         else
            raise Constraint_Error with
              "LHS in '/' is not a vector or scalar";
         end if;
      end "/";

   end Operators;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (V : in out Any) is
   begin
      if V.Concrete /= null then
         V.Concrete := new Any'Class'(V.Concrete.all);
      end if;
   end Adjust;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (This : Any'Class) return Integer is
   begin
      if This.Concrete /= null then
         return This.Concrete.As_Integer;
      else
         return Ada.Numerics.Big_Numbers.Big_Integers.To_Integer (Int (This).Value);
      end if;
   end As_Integer;

   ------------
   -- As_Map --
   ------------

   function As_Map (This : Any'Class) return access constant Map'Class
   is (if This.Concrete /= null
       then raise Program_Error with "inner pointer should never be not null for maps"
       else Map'Class (This)'Unchecked_Access);

   function As_Map (This : Any'Class; Key : String)
                    return access constant Any'Class
   is (Map'Class (This).Constant_Reference (Key));

   ---------------
   -- As_String --
   ---------------

   function As_String (This : Any'Class) return String
   is (if This.Concrete /= null
       then To_String (Str (This.Concrete.all).Value)
       else To_String (Str (This).Value));

   ------------
   -- As_Vec --
   ------------

   function As_Vec (This : Any'Class) return access constant Vec'Class
   is (if This.Concrete /= null
       then raise Program_Error with "inner pointer should never be not null for vecs"
       else Vec'Class (This)'Unchecked_Access);

   function As_Vec (This : Any'Class; Index : Positive)
                    return access constant Any'Class
   is (Vec'Class (This).Constant_Reference (Index));

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference (This : Any'Class; Pos : Positive)
                                return access constant Any'Class
   is (if This.Is_Vec
       then This.As_Vec.Value.Constant_Reference (Pos).Element
       else raise Constraint_Error with
         "Attemp to index into a non-vector, container is " & This.Tag);

   function Constant_Reference (This : Any'Class; Key : String)
                                return access constant Any'Class
   is (if This.Is_Map
       then This.As_Map.Value.Constant_Reference (Key).Element
       else raise Constraint_Error with
         "Attemp to select from a non-map, container is " & This.Tag);

   function Constant_Reference (This : Any'Class;
                                Path : Vec'Class)
                                return not null access constant Any'Class
   is
   begin
      if Path.Value.Is_Empty then
         raise Constraint_Error with "Attempt to index with empty vector";
      end if;

      declare
         Remaining_Keys : Vec'Class := Path;
         First_Key      : constant Any'Class := Path.Value.First_Element;
         First          : access constant Any'Class;
      begin
         Remaining_Keys.Value.Delete_First;

         if This in Map and then First_Key.Is_Str then
            First := Map (This).Constant_Reference (First_Key.As_String);
         elsif This in Vec and then First_Key.Is_Int then
            First := Vec (This).Constant_Reference (First_Key.As_Integer);
         else
            raise Constraint_Error with
              "Mismatch between container and index: "
              & "container is " & This.Tag & "; "
              & "index is " & First_Key.Tag;
         end if;

         if Remaining_Keys.Value.Is_Empty then
            return First;
         else
            return Constant_Reference (First.all, Remaining_Keys);
         end if;
      end;
   end Constant_Reference;

   ---------
   -- Get --
   ---------

   function Get (This : Vec; Indices : Vec'Class)
                 return not null access constant Any'Class
   is (This.Constant_Reference (Indices));

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (V : in out Any) is
      procedure Free is new Ada.Unchecked_Deallocation (Any'Class, Ptr);
   begin
       Free (V.Concrete);
   end Finalize;

   -----------
   -- Image --
   -----------

   function Image (V : Any) return String is
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

   ----------------
   -- JSON_Image --
   ----------------

   function JSON_Image (V : Any) return String is
   begin
      if V.Concrete /= null then
         return V.Concrete.JSON_Image;
      else
         return "null";
      end if;
   end JSON_Image;

   overriding function JSON_Image (V : Bool) return String is
   begin
      if V.Value then
         return "true";
      else
         return "false";
      end if;
   end JSON_Image;

   overriding function JSON_Image (V : Int) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Ada.Numerics.Big_Numbers.Big_Integers.To_String (V.Value),
         Side => Ada.Strings.Both);
   end JSON_Image;

   overriding function JSON_Image (V : Real) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Ada.Numerics.Big_Numbers.Big_Reals.To_String (V.Value),
         Side => Ada.Strings.Both);
   end JSON_Image;

   overriding function JSON_Image (V : Str) return String is
      Result : Unbounded_String := To_Unbounded_String ("""");
   begin
      for C of To_String (V.Value) loop
         case C is
            when Character'Val (16#00#) =>
               Append (Result, "\u0000");
            when Character'Val (16#01#) =>
               Append (Result, "\u0001");
            when Character'Val (16#02#) =>
               Append (Result, "\u0002");
            when Character'Val (16#03#) =>
               Append (Result, "\u0003");
            when Character'Val (16#04#) =>
               Append (Result, "\u0004");
            when Character'Val (16#05#) =>
               Append (Result, "\u0005");
            when Character'Val (16#06#) =>
               Append (Result, "\u0006");
            when Character'Val (16#07#) =>
               Append (Result, "\u0007");
            when Ada.Characters.Latin_1.BS =>
               Append (Result, "\b");
            when Ada.Characters.Latin_1.HT =>
               Append (Result, "\t");
            when Ada.Characters.Latin_1.LF =>
               Append (Result, "\n");
            when Character'Val (16#0B#) =>
               Append (Result, "\u000B");
            when Ada.Characters.Latin_1.FF =>
               Append (Result, "\f");
            when Ada.Characters.Latin_1.CR =>
               Append (Result, "\r");
            when Character'Val (16#0E#) =>
               Append (Result, "\u000E");
            when Character'Val (16#0F#) =>
               Append (Result, "\u000F");
            when Character'Val (16#10#) =>
               Append (Result, "\u0010");
            when Character'Val (16#11#) =>
               Append (Result, "\u0011");
            when Character'Val (16#12#) =>
               Append (Result, "\u0012");
            when Character'Val (16#13#) =>
               Append (Result, "\u0013");
            when Character'Val (16#14#) =>
               Append (Result, "\u0014");
            when Character'Val (16#15#) =>
               Append (Result, "\u0015");
            when Character'Val (16#16#) =>
               Append (Result, "\u0016");
            when Character'Val (16#17#) =>
               Append (Result, "\u0017");
            when Character'Val (16#18#) =>
               Append (Result, "\u0018");
            when Character'Val (16#19#) =>
               Append (Result, "\u0019");
            when Character'Val (16#1A#) =>
               Append (Result, "\u001A");
            when Character'Val (16#1B#) =>
               Append (Result, "\u001B");
            when Character'Val (16#1C#) =>
               Append (Result, "\u001C");
            when Character'Val (16#1D#) =>
               Append (Result, "\u001D");
            when Character'Val (16#1E#) =>
               Append (Result, "\u001E");
            when Character'Val (16#1F#) =>
               Append (Result, "\u001F");
            when '\' =>
               Append (Result, "\\");
            when '"' =>
               Append (Result, "\""");
            when others =>
               Append (Result, C);
         end case;
      end loop;

      Append (Result, """");

      return To_String (Result);
   end JSON_Image;

   overriding function JSON_Image (V : Map) return String is
      use Maps;
      Result : Unbounded_String := To_Unbounded_String ("{");
   begin
      for I in V.Value.Iterate loop
         Append (Result,
                 Str'(Controlled with
                      Concrete => null,
                      Value => To_Unbounded_String (Key (I))).JSON_Image &
                   ":" & Element (I).JSON_Image);
         if I /= V.Value.Last then
            Append (Result, ",");
         end if;
      end loop;

      Append (Result, "}");

      return To_String (Result);
   end JSON_Image;

   overriding function JSON_Image (V : Vec) return String is
      use Vectors;
      Result : Unbounded_String := To_Unbounded_String ("[");
   begin
      for I in V.Value.Iterate loop
         Append (Result, Element (I).JSON_Image);
         if I /= V.Value.Last then
            Append (Result, ",");
         else
            Append (Result, "]");
         end if;
      end loop;

      return To_String (Result);
   end JSON_Image;

   ---------------
   -- Is_Scalar --
   ---------------

   function Is_Scalar (This : Any'Class) return Boolean is
   begin
      return
        (This.Concrete /= null and then This.Concrete.Is_Scalar)
        or else This in Bool | Int | Real | Str;
   end Is_Scalar;

   ------------
   -- Length --
   ------------

   function Length (This : Vec) return Positive is (Positive (This.Value.Length));

   ---------
   -- Tag --
   ---------

   function Tag (This : Any'Class) return String
   is (Ada.Tags.External_Tag (This'Tag));

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Any is
   begin
      return (Controlled with Concrete =>
                 new Int'(Controlled with
                  Concrete => null,
                  Value    => Ada.Numerics.Big_Numbers.Big_Integers.From_String (Img)));
   end To_Int;

   -------------
   -- To_Real --
   -------------

   function To_Real (Img : String) return Any is
   begin
      return (Controlled with Concrete =>
                 new Real'(Controlled with
                  Concrete => null,
                  Value    => Ada.Numerics.Big_Numbers.Big_Reals.From_String (Img)));
   end To_Real;

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Any is
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
     (This : in out Map; Key : String; Value : Any'Class)
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

   procedure Append (This : in out Vec; Value : Any'Class) is
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
