pragma Ada_2022;

procedure Yeison_Tests.Deletions is

   -----------------
   -- Expect_Fail --
   -----------------

   --  Run Op and assert it raised; used for out-of-range / absent cases.

   procedure Expect_Fail (Op : access procedure; Msg : String) is
   begin
      Op.all;
      Assert (False, Msg);
   exception
      when others =>
         null; --  Expected
   end Expect_Fail;

begin
   --  Map: in-place key deletion, and insertion-order vector stays consistent
   declare
      M : Any := ["a" => 1, "b" => 2, "c" => 3];
   begin
      M.Delete ("b");

      Assert (M.Length = 2,            "map length after delete");
      Assert (not M.Has_Key ("b"),     "deleted key gone");
      Assert (M.Has_Key ("a") and then M.Has_Key ("c"), "others remain");

      --  Keys vector reflects the deletion, preserving order a, c
      declare
         K : constant Any := M.Keys;
      begin
         Assert (K.Length = 2,         "keys length after delete");
         Assert (K (1) = "a",          "first key still a");
         Assert (K (2) = "c",          "second key now c");
      end;
   end;

   --  Map: functional delete returns a copy, leaving the original intact
   declare
      M  : constant Any := ["x" => 1, "y" => 2];
      M2 : constant Any := M.Delete ("x");
   begin
      Assert (M.Length = 2,            "functional delete keeps original");
      Assert (M2.Length = 1,           "copy has one fewer");
      Assert (not M2.Has_Key ("x"),    "copy lost the key");
      Assert (M2.Has_Key ("y"),        "copy kept the other");
   end;

   --  Vector: in-place index deletion shifts remaining elements down
   declare
      V : Any := +[10, 20, 30];
   begin
      V.Delete (2);

      Assert (V.Length = 2,            "vector length after delete");
      Assert (V (1).As_Int = 10,       "first element unchanged");
      Assert (V (2).As_Int = 30,       "third element shifted down");
   end;

   --  Vector: functional delete returns a copy
   declare
      V  : constant Any := +[1, 2, 3];
      V2 : constant Any := V.Delete (1);
   begin
      Assert (V.Length = 3,            "functional delete keeps original");
      Assert (V2.Length = 2,           "copy is shorter");
      Assert (V2 (1).As_Int = 2,       "copy starts at former second");
   end;

   --  Clear empties a map but keeps it a map
   declare
      M : Any := ["a" => 1, "b" => 2];
   begin
      M.Clear;
      Assert (M.Kind = Map_Kind,       "cleared map is still a map");
      Assert (M.Is_Empty,              "cleared map is empty");
      Assert (M.Keys.Length = 0,       "cleared map has no keys");
   end;

   --  Clear empties a vector but keeps it a vector
   declare
      V : Any := +[1, 2];
   begin
      V.Clear;
      Assert (V.Kind = Vec_Kind,       "cleared vector is still a vector");
      Assert (V.Is_Empty,              "cleared vector is empty");
   end;

   --  Nested in-place mutation through indexing: the reference returned by
   --  variable indexing must mutate the *stored* nested element, not a copy.
   declare
      MoV : Any := ["v" => +[1, 2, 3]];                --  map of vectors
      VoV : Any := +[+[1, 2], +[3, 4]];                --  vector of vectors
      MoM : Any := ["inner" => ["a" => 1, "b" => 2]];  --  map of maps
   begin
      MoV ("v").Delete (2);
      Assert (MoV ("v").Length = 2,      "nested vec delete in place (in map)");
      Assert (MoV ("v") (2).As_Int = 3,  "nested vec shifted in place");

      VoV (1).Delete (1);
      Assert (VoV (1).Length = 1,        "nested vec delete in place (in vec)");
      Assert (VoV (1) (1).As_Int = 2,    "nested vec content after delete");

      MoM ("inner").Delete ("a");
      Assert (MoM ("inner").Length = 1,         "nested map delete in place");
      Assert (not MoM ("inner").Has_Key ("a"),  "nested map key gone");
      Assert (MoM ("inner").Has_Key ("b"),      "nested map sibling remains");

      MoV ("v").Clear;
      Assert (MoV ("v").Is_Empty,        "nested clear in place");
      Assert (MoV ("v").Kind = Vec_Kind, "nested clear keeps kind");
   end;

   --  Error cases
   declare
      Missing_Key : Any := ["a" => 1];
      Bad_Index   : Any := +[1];
      Wrong_Index : Any := +[1];

      procedure Del_Missing is
      begin
         Missing_Key.Delete ("nope");
      end Del_Missing;

      procedure Del_Out_Of_Range is
      begin
         Bad_Index.Delete (5);
      end Del_Out_Of_Range;

      procedure Del_Non_Integer is
      begin
         Wrong_Index.Delete ("key");
      end Del_Non_Integer;
   begin
      Expect_Fail (Del_Missing'Access,
                   "deleting an absent key should raise");
      Expect_Fail (Del_Out_Of_Range'Access,
                   "deleting out-of-range index should raise");
      Expect_Fail (Del_Non_Integer'Access,
                   "non-integer vector index should raise");
   end;
end Yeison_Tests.Deletions;
