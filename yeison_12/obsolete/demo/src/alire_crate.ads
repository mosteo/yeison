with Yeison; use Yeison;

package Alire_Crate is

   URL    : constant String := "url";
   Commit : constant String := "commit";

   Linux  : constant String := "linux";
   Other  : constant String := "...";

   Crate : constant Yeison.Map
     := [
         "name"          => "alr",
         "description"   => "The Alire project command-line tool",
         "version"       => "1.2.0-dev",
         "auto-gpr-with" => False,
         "maintainers"   => Vec'["mosteo", "chouteau"],

         "depends-on" =>
           Map'["aaa"      => "~0.2.3",
                "ada_toml" => "~0.1",
                "spdx"     => "~0.2"],

         "pins" =>
           Map'["ada_toml" => Map'[URL => "http://adatoml", Commit => "abcd"],
                "spdx"     => Map'[URL => "http://spdx",    Commit => "1234"]],

         "available" =>
           Map'["case(os)" =>
                  Map'[Linux => True,
                       Other => False]]
        ];

end Alire_Crate;
