package body Yeison_Daseot is

   ----------
   -- Make --
   ----------

   package body Make is

      ---------
      -- Vec --
      ---------

      function Vec (This : Yeison.Vec) return Any is (Any (This));

   end Make;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Any; Elem : Any) is
   begin
      This.Tree.Append (Elem.Tree);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Vec;
                     Elem : Any'Class)
   is
   begin
      Any (This).Append (Any (Elem));
   end Append;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This  : in out Any;
                         Key   : Text;
                         Value : Any)
   is
   begin
      This.Tree.Map (Key, Value.Tree);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert (This    : in out Any;
                     Key     : Any;
                     Value   : Any;
                     Replace : Boolean := False)
   is
   begin
      raise Unimplemented;
   end Insert;

   ---------------
   -- Reference --
   ---------------

   function Reference (This : aliased Any; Pos : Any) return Ref
   is

      procedure Constraint_Error (Msg : String; Pos : Any) with No_Return;

      ----------------------
      -- Constraint_Error --
      ----------------------

      procedure Constraint_Error (Msg : String; Pos : Any) is
      begin
         raise Standard.Constraint_Error
           with "cannot index " & Msg & " when index is " & Pos.Image;
      end Constraint_Error;

      ------------------
      -- Ref_By_Index --
      ------------------

      function Ref_By_Scalar (This : aliased Any; Pos : Any) return Ref is
      begin
         --  Initialize empty vec/map if needed
         if not This.Is_Valid then
            case Pos.Kind is
               when Int_Kind =>
                  This.Self.Tree := Trees.Empty_List;
               when Map_Kind =>
                  Constraint_Error ("null Any with map", Pos);
               when others =>
                  This.Self.Tree := Trees.Empty_Dict;
            end case;
         end if;

         --  Access the position. At this point Pos must be a scalar

         case This.Kind is
            when Scalar_Kinds =>
               if Pos.Kind /= Int_Kind or else Pos.As_Int /= 1 then
                  Constraint_Error ("scalar value with any /= 1", Pos);
               end if;

               return This.Self;

            when Map_Kind =>
               raise Unimplemented;

            when others =>
               raise Unimplemented;
         end case;
      end Ref_By_Scalar;

   begin
      case Pos.Kind is
         when Map_Kind =>
            Constraint_Error ("with a map", Pos);
         when Vec_Kind =>
            raise Unimplemented;
            --  Get first and continue indexing
         when Scalar_Kinds =>
            --  We can already return a reference
            return Ref_By_Scalar (This, Pos);
      end case;
   end Reference;

end Yeison_Daseot;
