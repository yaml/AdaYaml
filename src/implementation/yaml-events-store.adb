--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Store is
   function New_Store return Reference is
      Ptr : constant not null Instance_Access := new Instance;
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

   procedure Memorize (Object : in out Instance; Item : Event; Force : Boolean)
   is
      use type Text.Reference;
   begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      end if;
      case Item.Kind is
         when Annotation_Start =>
            if Item.Annotation_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include
                 (Item.Annotation_Properties.Anchor,
                  (Position => Object.Length + 1, Has_Been_Output => False));
            elsif Object.Depth = 0 and not Force then
               return;
            end if;
            if Object.Depth = After_Annotation_End then
               Object.Depth := 1;
            else
               Object.Depth := Object.Depth + 1;
            end if;
         when Scalar =>
            if Item.Scalar_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include
                 (Item.Scalar_Properties.Anchor,
                  (Position => Object.Length + 1,
                   Has_Been_Output => False));
            elsif Object.Depth = 0 and not Force then
               return;
            end if;
            if Object.Depth = After_Annotation_End then
               Object.Depth := 0;
            end if;
         when Mapping_Start =>
            if Item.Collection_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include
                 (Item.Collection_Properties.Anchor,
                  (Position => Object.Length + 1,
                   Has_Been_Output => False));
            elsif Object.Depth = 0 and not Force then
               return;
            end if;
            if Object.Depth = After_Annotation_End then
               Object.Depth := 1;
            else
               Object.Depth := Object.Depth + 1;
            end if;
         when Sequence_Start =>
            if Item.Collection_Properties.Anchor /= Text.Empty then
               Object.Anchor_Map.Include
                 (Item.Collection_Properties.Anchor,
                  (Position => Object.Length + 1,
                   Has_Been_Output => False));
            elsif Object.Depth = 0 and not Force then
               return;
            end if;
            if Object.Depth = After_Annotation_End then
               Object.Depth := 1;
            else
               Object.Depth := Object.Depth + 1;
            end if;
         when Mapping_End | Sequence_End =>
            if Object.Depth = 0 and not Force then
               return;
            end if;
            Object.Depth := Object.Depth - 1;
         when Annotation_End =>
            if Object.Depth = 0 and not Force then
               return;
            end if;
            Object.Depth := Object.Depth - 1;
            if Object.Depth = 0 then
               Object.Depth := After_Annotation_End;
            end if;
         when others =>
            if Object.Depth = 0 and not Force then
               return;
            elsif Object.Depth = After_Annotation_End then
               Object.Depth := 0;
            end if;
      end case;
      if Object.Length = Object.Data.all'Length then
         Object.Grow;
      end if;
      Object.Length := Object.Length + 1;
      Object.Data (Object.Length) := Item;
   end Memorize;

   procedure Memorize (Object : in out Instance; Item : Event) is
   begin
      Memorize (Object, Item, False);
   end Memorize;

   procedure Force_Memorize (Object : in out Instance; Item : Event;
                             Position : out Element_Cursor) is
   begin
      Memorize (Object, Item, True);
      Position := Element_Cursor (Object.Length);
   end Force_Memorize;

   function Find (Object : Instance; Alias : Text.Reference)
                  return Anchor_Cursor is
      (Anchor_Cursor (Object.Anchor_Map.Find (Alias)));

   function Exists_In_Output (Position : Anchor_Cursor) return Boolean is
     (Anchor_To_Index.Element
        (Anchor_To_Index.Cursor (Position)).Has_Been_Output);

   procedure Set_Exists_In_Output (Object : in out Instance;
                                   Position : Anchor_Cursor) is
      procedure Process (Key : Text.Reference;
                         Element : in out Anchor_Info) is
         pragma Unreferenced (Key);
      begin
         Element.Has_Been_Output := True;
      end Process;
   begin
      Anchor_To_Index.Update_Element (Object.Anchor_Map,
                                      Anchor_To_Index.Cursor (Position),
                                      Process'Access);
   end Set_Exists_In_Output;

   procedure Advance (Position : in out Element_Cursor) is
   begin
      Position := Element_Cursor'Succ (Position);
   end Advance;

   procedure Advance_At_Same_Level (Object : Instance;
                                    Position : in out Element_Cursor) is
      Depth : Natural := 0;
   begin
      loop
         case Object.Data (Positive (Position)).Kind is
            when Annotation_Start | Sequence_Start | Mapping_Start |
                 Document_Start =>
               Depth := Depth + 1;
            when Annotation_End =>
               Depth := Depth - 1;
            when Sequence_End | Mapping_End | Document_End =>
               Depth := Depth - 1;
               if Depth = 0 then
                  Position := Element_Cursor'Succ (Position);
                  return;
               end if;
            when Scalar | Alias =>
               if Depth = 0 then
                  Position := Element_Cursor'Succ (Position);
                  return;
               end if;
            when Stream_Start | Stream_End =>
               raise Stream_Error with "Unexpected event inside stream: " &
                 Object.Data (Positive (Position)).Kind'Img;
         end case;
         Position := Element_Cursor'Succ (Position);
      end loop;
   end Advance_At_Same_Level;

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

   function Retrieve (Object : Reference'Class; Position : Anchor_Cursor)
                         return Stream_Reference is
      Ptr : constant not null Stream_Instance_Access :=
        new Stream_Instance'(Refcount_Base with Object => Reference (Object),
                             Depth => 0, Current => Anchor_To_Index.Element
                               (Anchor_To_Index.Cursor (Position)).Position);
   begin
      Object.Data.Stream_Count := Object.Data.Stream_Count + 1;
      return Stream_Reference'(Ada.Finalization.Controlled with Data => Ptr);
   end Retrieve;

   function Retrieve (Object : Reference'Class; Position : Element_Cursor)
                      return Stream_Reference is
      Ptr : constant not null Stream_Instance_Access :=
        new Stream_Instance'(Refcount_Base with Object => Reference (Object),
                             Depth => 0, Current => Positive (Position));
   begin
      Object.Data.Stream_Count := Object.Data.Stream_Count + 1;
      return Stream_Reference'(Ada.Finalization.Controlled with Data => Ptr);
   end Retrieve;

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
         Object.Current := Object.Current + 1;
      end return;
   end Next;

   function Exists (Object : Optional_Stream_Reference) return Boolean is
     (Object.Data /= null);

   function Value (Object : Optional_Stream_Reference) return Stream_Accessor is
     ((Data => Object.Data));

   function Optional (Object : Stream_Reference'Class)
                      return Optional_Stream_Reference is
   begin
      Object.Data.Refcount := Object.Data.Refcount + 1;
      return (Ada.Finalization.Controlled with Data => Object.Data);
   end Optional;

   procedure Clear (Object : in out Optional_Stream_Reference) is
   begin
      if Object.Data /= null then
         Decrease_Refcount (Object.Data);
         Object.Data := null;
      end if;
   end Clear;

   function First (Object : Instance; Position : Anchor_Cursor) return Event is
     (Object.Data (Anchor_To_Index.Element (Anchor_To_Index.Cursor
                                            (Position)).Position));

   function Element (Object : Instance; Position : Element_Cursor)
                     return Event is
     (Object.Data (Positive (Position)));

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

   procedure Adjust (Object : in out Optional_Stream_Reference) is
   begin
      if Object.Data /= null then
         Increase_Refcount (Object.Data);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Optional_Stream_Reference) is
   begin
      if Object.Data /= null then
         Decrease_Refcount (Object.Data);
      end if;
   end Finalize;

   function To_Element_Cursor (Position : Anchor_Cursor) return Element_Cursor
   is (if Position = No_Anchor then No_Element else
          Element_Cursor (Anchor_To_Index.Element (Anchor_To_Index.Cursor
                                                   (Position)).Position));
end Yaml.Events.Store;
