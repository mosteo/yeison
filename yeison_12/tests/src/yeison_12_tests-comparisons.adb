procedure Yeison_12_Tests.Comparisons is
begin
   Assert (Sample ("one") = +"one"); -- any = any
   Assert (Sample ("one") = "one");  -- any = text
end Yeison_12_Tests.Comparisons;
