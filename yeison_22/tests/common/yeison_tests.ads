pragma Ada_2022;
pragma Warnings (Off);
with Ada.Assertions; use Ada.Assertions;
--  Make Assert visible to children

with Yeison; use Yeison;
with Yeison.Operators; use Yeison.Operators;

package Yeison_Tests is

   pragma Warnings (On);

   --  The same nested structure as the Yeison_12 testsuite, but built with the
   --  Ada 2022 literal and aggregate syntax instead of "+" constructors.

   function Sample return Yeison.Any
   is (["one"   => "one",
        "two"   => 2,
        "three" => +[1, "two", 3],
        "four"  => ["4a" => 4],
        "five"  => 5.5]);

end Yeison_Tests;
