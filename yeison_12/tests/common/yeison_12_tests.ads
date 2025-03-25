pragma Warnings (Off);
with Ada.Assertions; use Ada.Assertions;
--  Make Assert visible to children

with Yeison_12; use Yeison_12;
pragma Warnings (On);

package Yeison_12_Tests is

   use Operators;

   Sample : constant Any
     := Empty_Map
       .Insert (+"one", +"one")
       .Insert (+"two", +2)
       .Insert (+"three", To_Vec ((+1, +"two", +3)))
       .Insert (+"four", Empty_Map.Insert (+"4a", +4))
       .Insert (+"five", +5.5);

end Yeison_12_Tests;
