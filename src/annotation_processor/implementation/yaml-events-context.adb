--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Events.Context is
   use type Store.Optional_Reference;
   use type Text.Reference;
   use type Store.Anchor_Cursor;
   use type Store.Element_Cursor;

   procedure Free_Array is new Ada.Unchecked_Deallocation
     (Scope_Array, Scope_Array_Pointer);

   procedure Free_Symbol_Table is new Ada.Unchecked_Deallocation
     (Symbol_Tables.Map, Symbol_Table_Pointer);

   function Create (External : Store.Reference := Store.New_Store)
                    return Reference is
     ((Ada.Finalization.Controlled with Data =>
            new Instance'(Refcount_Base with Document_Data => Store.New_Store,
                          Stream_Data => Store.New_Store,
                          External_Data => External,
                          Local_Scopes => null, Local_Scope_Count => 0)));

   function External_Store (Object : Reference) return Store.Accessor is
     (Object.Data.External_Data.Value);

   function Stream_Store (Object : Reference) return Store.Accessor is
     (Object.Data.Stream_Data.Value);

   function Document_Store (Object : Reference) return Store.Accessor is
     (Object.Data.Document_Data.Value);

   function Local_Store (Object : Reference; Position : Local_Scope_Cursor)
                         return Store.Accessor is
   begin
      if (Object.Data.Local_Scopes = null or
        Object.Data.Local_Scope_Count < Natural (Position)) then
         raise Constraint_Error with "no local store at this position";
      elsif Object.Data.Local_Scopes (Positive (Position)).Events =
        Store.Null_Reference then
         Object.Data.Local_Scopes (Positive (Position)).Events :=
           Store.New_Store.Optional;
      end if;
      return Object.Data.Local_Scopes (Positive (Position)).Events.Value;
   end Local_Store;

   function Local_Store_Ref (Object : Reference; Position : Local_Scope_Cursor)
                             return Store.Optional_Reference is
   begin
      if (Object.Data.Local_Scopes = null or
            Object.Data.Local_Scope_Count < Natural (Position)) then
         return Store.Null_Reference;
      elsif Object.Data.Local_Scopes (Positive (Position)).Events =
        Store.Null_Reference then
         Object.Data.Local_Scopes (Positive (Position)).Events :=
           Store.New_Store.Optional;
      end if;
      return Object.Data.Local_Scopes (Positive (Position)).Events;
   end Local_Store_Ref;

   procedure Grow_Scopes (Object : in out Instance) is
   begin
      if Object.Local_Scopes = null then
         Object.Local_Scopes := new Scope_Array (1 .. 16);
      elsif Object.Local_Scope_Count = Object.Local_Scopes'Last then
         declare
            New_Array : constant not null Scope_Array_Pointer :=
              new Scope_Array (1 .. Object.Local_Scope_Count * 2);
         begin
            New_Array (Object.Local_Scopes'Range) :=
              Object.Local_Scopes.all;
            Free_Array (Object.Local_Scopes);
            Object.Local_Scopes := New_Array;
         end;
      end if;
      Object.Local_Scope_Count := Object.Local_Scope_Count + 1;
   end Grow_Scopes;

   procedure Create_Local_Store (Object : Reference;
                                 Position : out Local_Scope_Cursor) is
   begin
      Grow_Scopes (Object.Data.all);
      Object.Data.Local_Scopes (Object.Data.Local_Scope_Count).Events :=
        Store.New_Store.Optional;
      Position := Local_Scope_Cursor (Object.Data.Local_Scope_Count);
   end Create_Local_Store;

   procedure Create_Local_Symbol_Scope (Object : Reference;
                                        Position : out Local_Scope_Cursor) is
   begin
      Grow_Scopes (Object.Data.all);
      Object.Data.Local_Scopes (Object.Data.Local_Scope_Count).Symbols :=
        new Symbol_Tables.Map;
      Position := Local_Scope_Cursor (Object.Data.Local_Scope_Count);
   end Create_Local_Symbol_Scope;

   procedure Release_Local_Store (Object : Reference;
                                  Position : Local_Scope_Cursor) is
   begin
      if Object.Data.Local_Scopes (Positive (Position)).Symbols /= null then
         Free_Symbol_Table
           (Object.Data.Local_Scopes (Positive (Position)).Symbols);
      end if;
      Object.Data.Local_Scopes (Positive (Position)).Events :=
        Store.Null_Reference;
      while Object.Data.Local_Scope_Count > 0 and then
        (Object.Data.Local_Scopes (Object.Data.Local_Scope_Count).Events =
             Store.Null_Reference and Object.Data.Local_Scopes
           (Object.Data.Local_Scope_Count).Symbols = null) loop
         Object.Data.Local_Scope_Count := Object.Data.Local_Scope_Count - 1;
      end loop;
   end Release_Local_Store;

   procedure Create_Symbol (Object : Reference;
                            Scope  : Local_Scope_Cursor;
                            Name   : Text.Reference;
                            Position : out Symbol_Cursor) is
      Inserted : Boolean;
   begin
      if Object.Data.Local_Scopes (Positive (Scope)).Symbols = null then
         Object.Data.Local_Scopes (Positive (Scope)).Symbols :=
           new Symbol_Tables.Map;
      end if;
      Object.Data.Local_Scopes (Positive (Scope)).Symbols.Insert
        (Name, No_Element, Symbol_Tables.Cursor (Position), Inserted);
      if not Inserted then
         raise Constraint_Error with "Symbol """ & Name & """ already exists!";
      end if;
   end Create_Symbol;

   procedure Update_Symbol (Object : Reference;
                            Scope  : Local_Scope_Cursor;
                            Position : Symbol_Cursor;
                            New_Value : Cursor) is
      function Try_Update_With_Anchored (Anchor : Text.Reference)
                                         return Boolean is
      begin
         if Anchor = Text.Empty then
            return False;
         end if;
         Object.Data.Local_Scopes (Positive (Scope)).Symbols.Replace_Element
           (Symbol_Tables.Cursor (Position), Object.Position (Anchor));
         return True;
      end Try_Update_With_Anchored;

      Target_Event : constant Event := First (New_Value);
   begin
      case Target_Event.Kind is
         when Scalar =>
            if Try_Update_With_Anchored (Target_Event.Scalar_Properties.Anchor)
            then
               return;
            end if;
         when Mapping_Start | Sequence_Start =>
            if Try_Update_With_Anchored
              (Target_Event.Collection_Properties.Anchor) then
               return;
            end if;
         when Alias =>
            Object.Data.Local_Scopes (Positive (Scope)).Symbols.Replace_Element
              (Symbol_Tables.Cursor (Position),
               Object.Position (Target_Event.Target));
            return;
         when others => null;
      end case;
      Object.Data.Local_Scopes (Positive (Scope)).Symbols.Replace_Element
        (Symbol_Tables.Cursor (Position), New_Value);
   end Update_Symbol;

   function Position (Object : Reference; Alias : Text.Reference) return Cursor
   is
      Pos : Store.Anchor_Cursor := Store.No_Anchor;
   begin
      for Index in reverse 1 .. Object.Data.Local_Scope_Count loop
         if Object.Data.Local_Scopes (Index).Symbols /= null then
            declare
               Symbol_Pos : constant Symbol_Tables.Cursor :=
                 Object.Data.Local_Scopes (Index).Symbols.Find (Alias);
            begin
               if Symbol_Tables.Has_Element (Symbol_Pos) then
                  return Symbol_Tables.Element (Symbol_Pos);
               end if;
            end;
         end if;
         if Object.Data.Local_Scopes (Index).Events /= Store.Null_Reference then
            Pos := Object.Data.Local_Scopes (Index).Events.Value.Find (Alias);
            if Pos /= Store.No_Anchor then
               return (Target => Object.Data.Local_Scopes (Index).Events,
                       Anchored_Position => Pos,
                       Element_Position => Events.Store.No_Element,
                       Target_Location => Local);
            end if;
         end if;
      end loop;
      Pos := Object.Data.Document_Data.Value.Find (Alias);
      if Pos = Store.No_Anchor then
         Pos := Object.Data.Stream_Data.Value.Find (Alias);
         if Pos = Store.No_Anchor then
            Pos := Object.Data.External_Data.Value.Find (Alias);
            if Pos = Store.No_Anchor then
               return No_Element;
            else
               return (Target => Object.Data.External_Data.Optional,
                       Anchored_Position => Pos,
                       Element_Position => Events.Store.No_Element,
                       Target_Location => External);
            end if;
         else
            return (Target => Object.Data.Stream_Data.Optional,
                    Anchored_Position => Pos,
                    Element_Position => Events.Store.No_Element,
                    Target_Location => Stream);
         end if;
      else
         return (Target => Object.Data.Document_Data.Optional,
                 Anchored_Position => Pos,
                 Element_Position => Events.Store.No_Element,
                 Target_Location => Document);
      end if;
   end Position;

   function Location (Position : Cursor) return Location_Type is
     (Position.Target_Location);

   function Is_Anchored (Pos : Cursor) return Boolean is
     (Pos.Anchored_Position /= Store.No_Anchor);

   function Retrieve (Pos : Cursor) return  Store.Stream_Reference is
     (if Pos.Element_Position /= Store.No_Element then
         Pos.Target.Required.Retrieve (Pos.Element_Position) else
         Pos.Target.Required.Retrieve (Pos.Anchored_Position));

   function First (Pos : Cursor) return Event is
     (if Pos.Element_Position /= Store.No_Element then
         Pos.Target.Value.Element (Pos.Element_Position) else
         Pos.Target.Value.First (Pos.Anchored_Position));

   procedure Adjust (Object : in out Reference) is
   begin
      Object.Data.Increase_Refcount;
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Object.Data.Decrease_Refcount;
   end Finalize;

   function Exists_In_Ouput (Position : Cursor) return Boolean is
     (if Position.Element_Position = Store.No_Element then
         Store.Exists_In_Output (Position.Anchored_Position) else False);

   procedure Set_Exists_In_Output (Position : in out Cursor) is
   begin
      if Position.Anchored_Position /= Events.Store.No_Anchor then
         Store.Set_Exists_In_Output (Position.Target.Value,
                                     Position.Anchored_Position);
      end if;
   end Set_Exists_In_Output;

   procedure Finalize (Object : in out Instance) is
   begin
      if Object.Local_Scopes /= null then
         for Index in 1 .. Object.Local_Scope_Count loop
            if Object.Local_Scopes (Index).Symbols /= null then
               Free_Symbol_Table (Object.Local_Scopes (Index).Symbols);
            end if;
         end loop;
         Free_Array (Object.Local_Scopes);
      end if;
   end Finalize;

   procedure Get_Store_And_Cursor
     (Position : Cursor; Target : out Store.Optional_Reference;
      Element_Position : out Events.Store.Element_Cursor) is
   begin
      Target := Position.Target;
      if Position.Anchored_Position /= Store.No_Anchor then
         Element_Position := Store.To_Element_Cursor
           (Position.Anchored_Position);
      else
         Element_Position := Position.Element_Position;
      end if;
   end Get_Store_And_Cursor;

   function To_Cursor (Object : Reference;
                       Parent : Store.Optional_Reference;
                       Element_Position : Events.Store.Element_Cursor)
                       return Cursor is
     ((Target => Parent, Anchored_Position => Store.No_Anchor,
       Element_Position => Element_Position,
       Target_Location => (if Parent.Value.Data = Object.External_Store.Data then
          External elsif Parent.Value.Data = Object.Stream_Store.Data then
             Stream elsif Parent.Value.Data = Object.Document_Store.Data then
                Document elsif Parent.Value.Data = null then None else Local)));
end Yaml.Events.Context;
