--  Configuration for yeison_12 generated by Alire
pragma Restrictions (No_Elaboration_Code);
pragma Style_Checks (Off);

package Yeison_12_Config is
   pragma Pure;

   Crate_Version : constant String := "0.2.0-dev";
   Crate_Name : constant String := "yeison_12";

   Alire_Host_OS : constant String := "linux";

   Alire_Host_Arch : constant String := "x86_64";

   Alire_Host_Distro : constant String := "ubuntu";

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

end Yeison_12_Config;
