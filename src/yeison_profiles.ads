package Yeison_Profiles is

   --  Test the use of several function profiles with a single aspect

   type List is tagged private with
     Aggregate =>
       (Empty => Empty,
        Add_Unnamed => Append);

   function Empty return List is (null record);

   procedure Append (L : in out List; I : Integer);

   Procedure Append (L : in out List; S : String);

private

   type List is tagged null record;

end Yeison_Profiles;
