# JSON-like data initialization with Ada 2022 features

Ada 2022 is around the corner and some features are already [making its way](https://blog.adacore.com/ada-202x-support-in-gnat) into GNAT. I am always eager to test the limits of the language, so whenever there is a much-awaited feature available I jump on the opportunity to try it.

Ada is well-known for its strong typing, which makes every conversion explicit. I would not want that to change, but a consequence is the frequent use of "crutch" functions to convert between types that have the same representation (plain integers, strings). The most habitual example is to use the `"+"` operator. For example, in `GNATCOLL.VFS`, we can find:

```Ada
function "+" (S : Filesystem_String) return String;
```

This, later, shows up in code as 

```Ada
Str : constant String := +Some_Variable;
```

The seasoned Ada programmer is used to this and not confused by the seemingly no-op nature of a `"+"` operator. Things get a bit more uglier when mixed with the string concatenation operator `"&"`, as precedence precludes writing things like:

```Ada
Put_Line (+Var_1 & " is a kind of " & +Var_2);
```

This in turns prompts the use of more "empty" syntax, adding parentheses:

```
Put_Line (+Var_1 & " is a kind of " & (+Var_2));
```

Mind you, there is no way around this in Ada 2022 either, which is a small price to pay for knowing what is going on explicitly. However, there is a particular case in which avoiding such extra conversions makes sense: when doing initializations with literals.

In Ada, integer and real literals are `universal types`, with the compiler resolving which concrete type applies, without the need for explicit conversions. A similar thing happens for strings, and so we can write things like this:

```Ada
declare
   type My_Int is range 0 .. 9999;
   X : Integer := 3;
   Y : My_Int  := 3; 
   
   type My_Str is array (My_Int range <>) of Character;
   S1 : String := "Hello";
   S2 : My_Str := "World";
```

And there is no ambiguity nor explicit conversion required.

## User-defined literal initializations

Let us say we want to create

