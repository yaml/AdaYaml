--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Store is
   function New_Store return Reference is
      Ptr : constant not null access Instance := new Instance;
   begin
      return (Ada.Finalization.Controlled with Data => Ptr);
   end New_Store;

   function Value (Object : Reference) return Accessor is
     ((Data => Object.Data));

   function Value (Object : Optional_Reference) return Accessor is
     ((Data => Object.Data));

   function Optional (Object : Reference'Class) return Optional_Reference is
   begin
      Increase_Refcount (Object.Data);
      return (Ada.Finalization.Controlled with Data => Object.Data);
   end Optional;

   function Required (Object : Optional_Reference'Class) return Reference is
   begin
      Increase_Refcount (Object.Data);
      return (Ada.Finalization.Controlled with Data => Object.Data);
   end Required;

   procedure Memorize (Object : in out Instance; Item : Event) is
      use type Text.Reference;
   begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      end if;
      case Item.Kind is
         when Scalar =>
            if Item.Scalar_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include (Item.Scalar_Properties.Anchor,
                                          Object.Length);
            elsif Object.Depth = 0 then
               return;
            end if;
         when Mapping_Start =>
            if Item.Collection_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include (Item.Collection_Properties.Anchor,
                                          Object.Length);
            elsif Object.Depth = 0 then
               return;
            end if;
            Object.Depth := Object.Depth + 1;
         when Sequence_Start =>
            if Item.Collection_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include (Item.Collection_Properties.Anchor,
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

   function Position (Object : Instance; Alias : Text.Reference)
                      return Anchored_Position is
      use type Anchor_To_Index.Cursor;

      Pos : constant Anchor_To_Index.Cursor :=
        Object.Anchor_Map.Find (Alias);
   begin
      if Pos = Anchor_To_Index.No_Element then
         return No_Element;
      else
         return Anchored_Position (Anchor_To_Index.Element (Pos));
      end if;
   end Position;

   procedure Clear (Object : in out Instance) is
   begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      end if;
      Object.Anchor_Map.Clear;
      Object.Depth := 0;
   end Clear;

   procedure Copy (Source : in Instance; Target : in out Instance) is
   begin
      if Target.Data.all'Length /= Source.Data.all'Length then
         Target.Finalize;
         Target.Data := new Event_Array (Source.Data.all'Range);
      end if;
      Target.Data.all := Source.Data.all;
      Target.Length := Source.Length;
      Target.Anchor_Map := Source.Anchor_Map;
      Target.Depth := Source.Depth;
   end Copy;

   package body Iteration is
      function Retrieve (Object : Reference;
                         Position : Anchored_Position)
                         return Stream_Reference is
         Ptr : constant not null access Stream_Instance :=
           new Stream_Instance'(Refcount_Base with Object => Object,
                                Depth => 0, Current => Positive (Position));
      begin
         Object.Data.Stream_Count := Object.Data.Stream_Count + 1;
         return Stream_Reference'(Ada.Finalization.Controlled with Data => Ptr);
      end Retrieve;
   end Iteration;

   function Value (Object : Stream_Reference) return Stream_Accessor is
      ((Data => Object.Data));

   function Next (Object : in out Stream_Instance) return Event is
   begin
      if Object.Depth = 1 then
         raise Constraint_Error with
           "tried to query item after end of anchored node";
      end if;
      return Item : constant Event := Object.Object.Data.Data (Object.Current) do
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

   procedure Adjust (Object : in out Optional_Reference) is
   begin
      if Object.Data /= null then
         Increase_Refcount (Object.Data);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Optional_Reference) is
   begin
      if Object.Data /= null then
         Decrease_Refcount (Object.Data);
      end if;
   end Finalize;

   procedure Finalize (Object : in out Stream_Instance) is
   begin
      Object.Object.Data.Stream_Count := Object.Object.Data.Stream_Count - 1;
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
