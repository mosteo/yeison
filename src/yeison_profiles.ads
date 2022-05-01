package Yeison_Profiles is

   --  Test the use of several function profiles with a single aspect

   type List is tagged private with
     Aggregate =>
       (Empty => Empty,
        Add_Unnamed => Append);

   function Empty return List is (null record);

   procedure Append (L : in out List; I : Integer);

   Procedure Append (L : in out List; S : String);

   type Base is tagged null record;
   type Int is new Base with record
      I : Integer;
   end record;

   --  function Make return Base'Class is ((I => 1)); Fails with "Type of
   --  aggregate cannot be class-wide". This is the same problem afflicting
   --  our initializations of maps.

   function Make return Base'Class is (Int'(I => 1));

private

   type List is tagged null record;

end Yeison_Profiles;
