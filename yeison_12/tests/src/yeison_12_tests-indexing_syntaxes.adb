procedure Yeison_12_Tests.Indexing_Syntaxes is
   Sample : Any renames Yeison_12_Tests.Sample;
begin
   Assert (Sample ("one").As_Text = "one");
   Assert (Sample ("one").Get = "one");
   Assert (Sample ("one") = +"one"); -- any = any
end Yeison_12_Tests.Indexing_Syntaxes;
