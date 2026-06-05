[![Alire indexed](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/yeison.json)](https://alire.ada.dev/crates/yeison)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)


# YEISON #

Just another "because we can™" library by yours truly.

Yeison is a heterogeneous container for general values, essentially equivalent to
the data models behind JSON, TOML and YAML. It is also an experiment in pushing
the new Ada 2022 initialization features to their limits.

It comes in two independent implementations:

- **`yeison` (Ada 2022)** — the experimental version, shown throughout this
  README. It leans on Ada 2022 features (user-defined literals, container
  aggregates, generalized indexing) for natural initialization and access.
  It requires a recent GNAT, such as the ones packaged in
  [Alire](https://alire.ada.dev).
- **`yeison_12` (Ada 2012)** — the same data structure and API for toolchains
  without Ada 2022. The values and operations are identical, but construction is
  more verbose (`+` constructor operators and `Empty_Map.Insert (...)` chains)
  since the literal and aggregate aspects are not available. This is the one
  used by Alire itself (since Alire is an Ada 2012 project).

## Values and initialization

Scalars come straight from literals; maps use `[key => value]` aggregates and
vectors use `+[...]`:

```Ada
with Yeison;           use Yeison;
with Yeison.Operators; use Yeison.Operators;

package Examples is

   Number   : constant Yeison.Int  := 3;
   Negative : constant Yeison.Int  := -3;
   Pi       : constant Yeison.Real := 3.14;
   Text     : constant Yeison.Str  := "Unicode welcome: Mi ĝuas la Adan programlingvon";

   Vec : constant Yeison.Vec := +[1, 2.0, "three"];
   --  A heterogeneous vector

   Map : constant Yeison.Map := ["1" => 1, "2" => 2.0, "3" => "three"];

   --  Maps and vectors nest arbitrarily
   Tree : constant Yeison.Map :=
     ["scalar" => 1,
      "vector" => +[1, 2, 3],
      "nested" => ["a" => 1, "b" => +["x", "y"]]];

end Examples;
```

`Yeison.Any` is the actual type; `Int`, `Real`, `Str`, `Vec` and `Map` are
convenience subtypes. A single value can hold any kind and be reassigned to
another kind later.

## Access and update

Indexing uses ordinary call notation. Nested access can chain calls, use the `/`
path operator, or pass a vector of indices:

```Ada
pragma Assert (Vec (2)   = 2.0);
pragma Assert (Map ("3") = "three");

pragma Assert (Tree ("vector") (2)   = 2);
pragma Assert (Tree ("vector" / 2)   = 2);   -- path operator
pragma Assert (Tree (+["vector", 2]) = 2);   -- vector of indices
pragma Assert (Tree ("nested") ("b") (1) = "x");
```

Values are mutable in place, and indexing a `Nil` value auto-vivifies the right
container (a map for a string key, a vector for an integer index):

```Ada
declare
   M : Yeison.Any;            -- starts Nil
begin
   M ("name")     := "alr";
   M ("versions") := +[1, 2, 3];
   M ("versions") (4) := 4;   -- grow a vector one past its end
end;
```

Entries are removed with `Delete` (a map key or a vector index; vector
elements shift down to stay contiguous) and a whole container is emptied in
place with `Clear`:

```Ada
M.Delete ("name");       -- drop a map entry by key
M ("versions").Delete (4);  -- drop a vector element by index
M.Clear;                 -- empty the map, keeping it a map
```

Both `Delete` forms come in a procedural (in-place) and a functional variant;
the latter returns a modified copy and leaves the original untouched.

Vectors additionally support `Append` and `Prepend` (each in-place and
functional). `Contains` tests value membership for either kind — vector
elements or map values — while `Has_Key` tests map keys:

```Ada
V.Prepend (0);                       -- add at the front
pragma Assert (V.Contains (2));      -- a vector element
pragma Assert (M.Contains ("alr"));  -- a map value (Has_Key tests keys)
```

Iteration uses the standard `for ... of` loop, and any value renders as Ada-like
or JSON text:

```Ada
for Value of Map loop
   Ada.Wide_Wide_Text_IO.Put_Line (Value.Image);          -- Ada-like
end loop;

Ada.Wide_Wide_Text_IO.Put_Line (Map.Image (JSON));        -- JSON
```

More examples live in the `tests` subfolder, which is itself an Alire crate you
can build and run with `alr test`.

## Numeric ranges and storage

Numbers are stored as 64-bit integers and 64-bit reals, and strings as Unicode
(`Wide_Wide_String`). This deliberately matches what the target formats actually
guarantee: TOML integers are 64-bit and its floats binary64, and JSON only
interoperates within IEEE-754 double.

# Using Yeison in your project

Yeison is indexed in Alire, so a published release is one command away:

```
alr with yeison        # Ada 2022 version
alr with yeison_12     # Ada 2012 version
```

The published releases lag behind, however. To get the newest features, track
the repository directly:

```
alr with yeison --use=https://github.com/mosteo/yeison
```

The `main` branch is kept green: both libraries build under
`alr build --validation` and `alr test` passes for each, so tracking it is
safe, although the API is subject to change.
