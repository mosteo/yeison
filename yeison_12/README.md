[![Alire indexed](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/yeison.json)](https://alire.ada.dev/crates/yeison)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)


# YEISON #

Just another "because we can™" library by yours truly.

Yeison is an heterogeneous container for general types, essentially equivalent
to JSON, TOML, YAML text file representations. It is also an experiment on
using new Ada 2022 features to enable simpler initializations and pushing them
to their limits. As such, it requires the latest GNAT compiler you can find
packaged in [Alire](https://alire.ada.dev).

With this library, these kind of declarations are possible:

```Ada
with Yeison;

package Examples is

   Number : constant Yeison.Int  := 3;
   Real   : constant Yeison.Real := 3.14;
   Text   : constant Yeison.Str  := "Some Unicode text: Mi ĝuas la Adan programlingvon";

   Vector : constant Yeison.Vec := (1, 2.0, "three");
   Map    : constant Yeison.Map := ("1" => 1, "2" => 2.0, "3" => "three");

   V2     : constant Yeison.Vec := (Vector, Map, Number, Real, Text);
   --  With nested vectors and maps inside a root vector

   M2     : constant Yeison.Map := ("vec" => V2, "map" => Map);
   --  With nested vectors and maps inside a root map

   M3     : constant Yeison.Map := ("one" => 1, 
                                    "two" => Yeison.Map'(2, Yeison.Vec'(3, 4, 5)));
   --  Inline initialization of nested structures

end Examples;
```

More examples available in the `demo` subfolder (which is also an Alire crate
so you can `alr run` it).

Accessing the values in these containers is done with the usual notation but
also relying on Yeison vectors for multilevel indexing:

```Ada
pragma Assert (Vector (2)        = 2.0);
pragma Assert (Map ("3")         = "three");
pragma Assert (V2  (1)(2)        = 2.0);
pragma Assert (V2 ((1, 2))       = 2.0);
pragma Assert (M2  ("map")("3")  = "three");
pragma Assert (M2 (("map", "3")) = "three");
```

Since this library has not been conceived with extreme efficiency in mind, but
to allow flexible use, it internally stores numbers as Big_Integers or
Big_Reals, and strings as Unicode, so at least you do not have to worry about
limits.

# Using Yeison in your projects

At the time Yeison has no dependencies, so you can download it and use it
normally as any other GNAT project through the provided GPR project file.

If using Alire, since it is not yet indexed, you can add it as a dependency of
your project with 
```
alr with --use=https://github.com/mosteo/yeison
```
