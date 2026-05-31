pragma Ada_2022;

procedure Yeison_Tests.Crate is

   URL    : constant Text := "url";
   Commit : constant Text := "commit";

   --  A realistic, deeply nested structure (an Alire crate manifest), written
   --  entirely with literal/aggregate syntax.

   Crate : constant Yeison.Map :=
     ["name"          => "alr",
      "description"   => "The Alire project command-line tool",
      "version"       => "1.2.0-dev",
      "auto-gpr-with" => False,
      "maintainers"   => +["mosteo", "chouteau"],

      "depends-on" =>
        ["aaa"      => "~0.2.3",
         "ada_toml" => "~0.1",
         "spdx"     => "~0.2"],

      "pins" =>
        Map'["ada_toml" => [URL => "http://adatoml", Commit => "abcd"],
             "spdx"     => [URL => "http://spdx",    Commit => "1234"]],

      "available" =>
        Map'["case(os)" =>
               Map'["linux" => True,
                    "..."   => False]]];

begin
   Assert (Crate.Kind = Map_Kind,                "crate is a map");
   Assert (Crate ("name").As_Text = "alr",       "crate name");
   Assert (Crate ("auto-gpr-with").Is_False,     "boolean field");

   Assert (Crate ("maintainers").Kind = Vec_Kind, "maintainers vector");
   Assert (Crate ("maintainers").Length = 2,      "two maintainers");
   Assert (Crate ("maintainers") (1).As_Text = "mosteo", "first maintainer");

   --  Deep nested access, chained and path forms
   Assert (Crate ("depends-on") ("ada_toml").As_Text = "~0.1",
           "nested dependency version");
   --  Index with string literals (a Text variable like URL is not a literal,
   --  so the String_Literal aspect would not apply to it when indexing).
   Assert (Crate ("pins") ("spdx") ("url").As_Text = "http://spdx",
           "deeply nested pin url");
   Assert (Crate ("available" / "case(os)" / "linux").Is_True,
           "path access to availability");
end Yeison_Tests.Crate;
