pragma Warnings (Off);
with Ada.Assertions; use Ada.Assertions;
--  Make Assert visible to children

with Yeison_12; use Yeison_12;

package Yeison_12_Tests is

   use Operators;

   pragma Warnings (On);

   --  Having a constant Any here causes memory corruption during package
   --  finalization. The problem appears once Iterable is added, even with
   --  a null cursor, so it seems to be a problem in GNAT itself (?).

   function Sample return Any
   is (Empty_Map
       .Insert (+"one", +"one")
       .Insert (+"two", +2)
       .Insert (+"three", To_Vec ((+1, +"two", +3)))
       .Insert (+"four", Empty_Map.Insert (+"4a", +4))
       .Insert (+"five", +5.5));

end Yeison_12_Tests;
