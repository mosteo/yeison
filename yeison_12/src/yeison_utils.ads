package Yeison_Utils with Preelaborate is

   --  Miscellanea supporting code of maybe interest elsewhere

   subtype Text is Wide_Wide_String;

   function JSON_Escape (Str : Text) return Text;
   --  Prepare a string for storage in JSON format. Does not add enclosing
   --  quotes!

end Yeison_Utils;
