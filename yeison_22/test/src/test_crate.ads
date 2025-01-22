with Yeison; use Yeison; use Yeison.Operators;

package Test_Crate with Elaborate_Body is

   subtype Text is Wide_Wide_String;

   URL    : constant Text := "url";
   Commit : constant Text := "commit";

   Linux  : constant Text := "linux";
   Other  : constant Text := "...";

   Crate : constant Yeison.Map
     := [
         "name"          => "alr",
         "description"   => "The Alire project command-line tool",
         "version"       => "1.2.0-dev",
         "auto-gpr-with" => False,
         "maintainers"   => +["mosteo", "chouteau"],

         "depends-on" =>
           Map'["aaa"      => "~0.2.3",
                "ada_toml" => "~0.1",
                "spdx"     => "~0.2"],

         "pins" =>
           Map'["ada_toml" => [URL => "http://adatoml", Commit => "abcd"],
                "spdx"     => [URL => "http://spdx",    Commit => "1234"]],

         "available" =>
           Map'["case(os)" =>
                  Map'[Linux => True,
                       Other => False]]
        ];

end Test_Crate;
