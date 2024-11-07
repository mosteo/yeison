private with Ada.Strings.Wide_Wide_Unbounded;

private with Daseot;

package Yeison_Daseot with Preelaborate is

   type Kinds is (Str_Kind);

   type Any is tagged private with
     String_Literal  => To_Str;

   function Invalid return Any;
   --  An uninitialized Any; using it as the RHS of assignments will fail

   function Is_Valid (This : Any) return Boolean;

   function Kind (This : Any) return Kinds with
     Pre => This.Is_Valid;

   function To_Str (Img : Wide_Wide_String) return Any;

   subtype Str is Any with
     Dynamic_Predicate => Str.Kind = Str_Kind;

private

   Unimplemented : exception;

   subtype WWUString
     is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   type Impls (Kind : Kinds) is record
      case Kind is
         when Str_Kind =>
            Str_Val : WWUString;
      end case;
   end record;

   package Trees is new Daseot (Impls, Impls'Image);

   type Any is tagged null record;

   -------------
   --  Impls  --
   -------------

   -------------
   -- Invalid --
   -------------

   function Invalid return Any is (null record);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Any) return Boolean
   is (This /= Invalid);

   ----------
   -- Kind --
   ----------

   function Kind (This : Any) return Kinds
   is (raise Unimplemented);

   ------------
   -- To_Str --
   ------------

   function To_Str (Img : Wide_Wide_String) return Any
   is (null record);

end Yeison_Daseot;
