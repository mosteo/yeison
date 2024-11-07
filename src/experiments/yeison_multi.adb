pragma Ada_2012;
package body Yeison_Multi is

   ------------
   -- To_Int --
   ------------

   function To_Int (Img : String) return Scalar is
   begin
      return raise Program_Error with "Unimplemented function To_Int";
   end To_Int;

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Scalar is
   begin
      return raise Program_Error with "Unimplemented function To_Str";
   end To_Str;

   -----------
   -- Empty --
   -----------

   function Empty return Map is
   begin
      return raise Program_Error with "Unimplemented function Empty";
   end Empty;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out Map; Key : String; Val : Any'Class) is
   begin
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out Map; Key : String; Val : Scalar'Class) is
   begin
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out Map; Key : String; Val : Map) is
   begin
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

end Yeison_Multi;
