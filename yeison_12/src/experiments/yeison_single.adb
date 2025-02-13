with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.IO; use GNAT.IO;

package body Yeison_Single is

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Any is
   begin
      return To_Holder (Inner_Int'(Value => Integer'Value (Img)));
   end To_Int;

   -------------
   -- To_Real --
   -------------

   function To_Real (Img : String) return Any is
   begin
      return To_Holder (Inner_Real'(Value => Float'Value (Img)));
   end To_Real;

   ---------------
   -- To_String --
   ---------------

   function To_String (Img : Wide_Wide_String) return Any is
   begin
      return To_Holder (Inner_Str'(Value => new Text'(Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Img))));
   end To_String;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (This : Any; Pos : Positive) return access constant Any
   is
   begin
      raise Constraint_Error;
      return Constant_Reference (This, Pos);
   end Constant_Reference;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (This : Any; Key : String) return access constant Any
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Constant_Reference unimplemented");
      return
        raise Program_Error with "Unimplemented function Constant_Reference";
   end Constant_Reference;

   -----------
   -- Empty --
   -----------

   function Empty return Any is
   begin
      return To_Holder (Inner_Map'(Value => <>));
   end Empty;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out Any; Key : String; Val : Any) is
      Inner : Inner_Map renames Inner_Map (This.Reference.Element.all);
   begin
      Inner.Value.Insert (Key, Val.Element);
   end Insert;

   ----------
   -- True --
   ----------

   function True return Any is
   begin
      return To_Holder (Inner_Bool'(Value => True));
   end True;

   -----------
   -- False --
   -----------

   function False return Any is
   begin
      return To_Holder (Inner_Bool'(Value => False));
   end False;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Inner_Map) Return String is
      use Inner_Maps;
      Result : Unbounded_String;
   begin
      Result := Result & "(";

      for I in This.Value.Iterate loop
         Result := Result & Key (I) & " => " & Element (I).Image;
         if I /= This.Value.Last then
            Result := Result & ", ";
         end if;
      end loop;

      Result := Result & ")";
      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Inner_Vec) return String is
      use Inner_Vectors;
      Result : Unbounded_String;
   begin
      Result := Result & "(";

      for I in This.Value.Iterate loop
         Result := Result & Element (I).Image;
         if I /= This.Value.Last then
            Result := Result & ", ";
         end if;
      end loop;

      Result := Result & ")";
      return To_String (Result);
   end Image;

   function Empty return Vec_Aux
   is (Value => (Value => Inner_Vectors.Empty_Vector));

   procedure Append (This : in out Vec_Aux; Val : Any) is
   begin
      This.Value.Value.Append (Val.Element);
   end Append;

   package body Operators is

      function "+" (This : Vec_Aux) return Any is
      begin
         return To_Holder (This.Value);
      end "+";

   end Operators;

end Yeison_Single;
