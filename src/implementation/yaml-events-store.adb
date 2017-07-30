--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Store is
   procedure Memorize (Object : in out Instance; Item : Event) is
      use type Text.Reference;
   begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      end if;
      case Item.Kind is
         when Scalar =>
            if Item.Scalar_Properties.Tag /= Text.Empty then
               Object.Tag_Map.Include (Item.Scalar_Properties.Tag,
                                       Object.Length);
            elsif Object.Depth = 0 then
               return;
            end if;
         when Mapping_Start =>
            if Item.Collection_Properties.Tag /= Text.Empty then
               Object.Tag_Map.Include (Item.Collection_Properties.Tag,
                                       Object.Length);
            elsif Object.Depth = 0 then
               return;
            end if;
            Object.Depth := Object.Depth + 1;
         when Sequence_Start =>
            if Item.Collection_Properties.Tag /= Text.Empty then
               Object.Tag_Map.Include (Item.Collection_Properties.Tag,
                                       Object.Length);
            elsif Object.Depth = 0 then
               return;
            end if;
            Object.Depth := Object.Depth + 1;
         when Mapping_End | Sequence_End =>
            if Object.Depth = 0 then
               return;
            end if;
            Object.Depth := Object.Depth - 1;
         when others =>
            if Object.Depth = 0 then
               return;
            end if;
      end case;
      if Object.Length = Object.Data.all'Length then
         Object.Grow;
      end if;
      Object.Length := Object.Length + 1;
      Object.Data (Object.Length) := Item;
   end Memorize;

   function Position (Object : Instance; Tag : Text.Reference)
                      return Anchored_Position is
      use type Tag_To_Index.Cursor;

      Pos : constant Tag_To_Index.Cursor :=
        Object.Tag_Map.Find (Tag);
   begin
      if Pos = Tag_To_Index.No_Element then
         return No_Element;
      else
         return Anchored_Position (Tag_To_Index.Element (Pos));
      end if;
   end Position;

   package body Iteration is
      function Retrieve (Object : not null access Instance;
                         Position : Anchored_Position)
                         return Stream_Reference is
         Ptr : constant not null access Stream_Instance :=
           new Stream_Instance'(Refcount_Base with Object => Object,
                                Depth => 0, Current => Positive (Position));
      begin
         Increase_Refcount (Object);
         Object.Stream_Count := Object.Stream_Count + 1;
         return Stream_Reference'(Ada.Finalization.Controlled with Data => Ptr);
      end Retrieve;
   end Iteration;

   function Next (Object : in out Stream_Instance) return Event is
   begin
      if Object.Depth = 1 then
         raise Constraint_Error with
           "tried to query item after end of anchored node";
      end if;
      return Item : constant Event := Object.Object.Data (Object.Current) do
         case Item.Kind is
            when Scalar => Object.Depth := Natural'Max (1, Object.Depth);
            when Mapping_Start | Sequence_Start =>
               Object.Depth := Natural'Max (2, Object.Depth + 1);
            when others => null;
         end case;
      end return;
   end Next;

   procedure Adjust (Object : in out Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;

   procedure Finalize (Object : in out Stream_Instance) is
   begin
      Object.Object.Stream_Count := Object.Object.Stream_Count - 1;
      Decrease_Refcount (Object.Object);
   end Finalize;

   procedure Adjust (Object : in out Stream_Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Stream_Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;
end Yaml.Events.Store;
