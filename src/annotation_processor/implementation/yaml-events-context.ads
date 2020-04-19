--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Hashed_Maps;
with Yaml.Events.Store;

package Yaml.Events.Context is
   type Reference is tagged private;

   type Cursor is private;

   type Local_Scope_Cursor is private;
   type Generated_Store_Cursor is private;

   type Symbol_Cursor is private;

   type Location_Type is (Generated, Local, Document, Stream, External, None);

   function Create (External : Store.Reference := Store.New_Store)
                    return Reference;

   function External_Store (Object : Reference) return Store.Accessor;
   function Stream_Store (Object : Reference) return Store.Accessor;
   function Document_Store (Object : Reference) return Store.Accessor;
   function Transformed_Store (Object : Reference) return Store.Accessor;
   function Local_Store (Object : Reference; Position : Local_Scope_Cursor)
                         return Store.Accessor;
   function Local_Store_Ref (Object : Reference; Position : Local_Scope_Cursor)
                             return Store.Optional_Reference;
   function Generated_Store (Object : Reference;
                             Position : Generated_Store_Cursor)
                             return Store.Accessor;
   function Generated_Store_Ref (Object : Reference;
                                 Position : Generated_Store_Cursor)
                                 return Store.Optional_Reference;

   function Position (Object : Reference; Alias : Text.Reference) return Cursor;
   function Location (Position : Cursor) return Location_Type;

   procedure Create_Local_Store (Object : Reference;
                                 Position : out Local_Scope_Cursor);
   procedure Create_Local_Symbol_Scope (Object : Reference;
                                        Position : out Local_Scope_Cursor);
   procedure Release_Local_Store (Object : Reference;
                                  Position : Local_Scope_Cursor);

   procedure Create_Generated_Store (Object : Reference;
                                     Position : out Generated_Store_Cursor);
   procedure Release_Generated_Store (Object : Reference;
                                      Position : Generated_Store_Cursor);

   procedure Create_Symbol (Object : Reference;
                            Scope  : Local_Scope_Cursor;
                            Name   : Text.Reference;
                            Position : out Symbol_Cursor);

   procedure Update_Symbol (Object    : Reference;
                            Scope     : Local_Scope_Cursor;
                            Position  : Symbol_Cursor;
                            New_Value : Cursor);

   function Symbol_Name (Position : Symbol_Cursor) return Text.Reference;

   No_Element : constant Cursor;
   No_Local_Store : constant Local_Scope_Cursor;

   function Is_Anchored (Pos : Cursor) return Boolean;

   function Retrieve (Pos : Cursor) return Store.Stream_Reference
     with Pre => Pos /= No_Element;

   function First (Pos : Cursor) return Event with Pre => Pos /= No_Element;

   function Exists_In_Ouput (Position : Cursor) return Boolean;
   procedure Set_Exists_In_Output (Position : in out Cursor);

   procedure Get_Store_And_Cursor
     (Position : Cursor; Target : out Store.Optional_Reference;
      Element_Position : out Events.Store.Element_Cursor);

   function To_Cursor (Object : Reference;
                       Parent : Store.Optional_Reference;
                       Element_Position : Events.Store.Element_Cursor)
                       return Cursor;
private
   type Cursor is record
      Target : Store.Optional_Reference;
      Anchored_Position : Events.Store.Anchor_Cursor;
      Element_Position  : Events.Store.Element_Cursor;
      Target_Location : Location_Type;
   end record;

   package Symbol_Tables is new Ada.Containers.Hashed_Maps
     (Text.Reference, Cursor, Text.Hash, Text."=");

   type Symbol_Table_Pointer is access Symbol_Tables.Map;

   type Local_Scope is record
      Events  : Store.Optional_Reference;
      Symbols : Symbol_Table_Pointer;
   end record;

   type Scope_Array is array (Positive range <>) of Local_Scope;
   type Scope_Array_Pointer is access Scope_Array;

   type Data_Array is array (Positive range <>) of Store.Optional_Reference;
   type Data_Array_Pointer is access Data_Array;

   type Instance is limited new Refcount_Base with record
      Generated_Data : Data_Array_Pointer;
      Document_Data, Stream_Data, External_Data, Transformed_Data :
        Store.Reference;
      Local_Scopes : Scope_Array_Pointer := null;
      Local_Scope_Count, Generated_Data_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   overriding procedure Finalize (Object : in out Instance);

   type Local_Scope_Cursor is new Natural;
   type Generated_Store_Cursor is new Natural;

   type Symbol_Cursor is new Symbol_Tables.Cursor;

   type Reference is new Ada.Finalization.Controlled with record
      Data : not null Instance_Access := raise Constraint_Error with "uninitialized context instance!";
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   No_Element : constant Cursor :=
     (Target => Store.Null_Reference,
      Element_Position => Events.Store.No_Element,
      Anchored_Position => Events.Store.No_Anchor,
      Target_Location => None);

   No_Local_Store : constant Local_Scope_Cursor := 0;
end Yaml.Events.Context;
