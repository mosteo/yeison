procedure Yeison_12_Tests.Comparisons is
   Sample : Any renames Yeison_12_Tests.Sample;
begin
   Assert (Sample ("one") = +"one"); -- any = any
   Assert (Sample ("one") = "one");  -- any = text
   Assert ("one" = Sample ("one"));  -- text = any

   Assert (Sample ("two") = 2);      -- any = big_int
   Assert (2 = Sample ("two"));      -- big_int = any

   Assert (Make.True.Is_True,           "Is_True: true value");
   Assert (Make.False.Is_False,         "Is_False: false value");
   Assert (not Make.True.Is_False,      "Is_False: rejects true value");
   Assert (not Make.False.Is_True,      "Is_True: rejects false value");
end Yeison_12_Tests.Comparisons;
