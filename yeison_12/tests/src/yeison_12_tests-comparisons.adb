procedure Yeison_12_Tests.Comparisons is
   Sample : Any renames Yeison_12_Tests.Sample;
begin
   Assert (Sample ("one") = +"one"); -- any = any
   Assert (Sample ("one") = "one");  -- any = text
end Yeison_12_Tests.Comparisons;
