--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Events.Store;

package Yaml.Events.Context is
   type Reference is tagged private;

   type Cursor is private;

   type Location_Type is (Document, Stream, External, None);

   function Create (External : Store.Reference := Store.New_Store)
                    return Reference;

   function External_Store (Object : Reference) return Store.Accessor;
   function Stream_Store (Object : Reference) return Store.Accessor;
   function Document_Store (Object : Reference) return Store.Accessor;

   function Position (Object : Reference; Alias : Text.Reference) return Cursor;
   function Location (Position : Cursor) return Location_Type;

   No_Element : constant Cursor;

   function Retrieve (Pos : Cursor) return Store.Stream_Reference
     with Pre => Pos /= No_Element;

   function First (Pos : Cursor) return Event with Pre => Pos /= No_Element;

   function Exists_In_Ouput (Position : Cursor) return Boolean;
   procedure Set_Exists_In_Output (Position : in out Cursor);
private
   type Instance is limited new Refcount_Base with record
      Document_Data, Stream_Data, External_Data : Store.Reference;
   end record;

   type Reference is new Ada.Finalization.Controlled with record
      Data : not null access Instance;
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   type Cursor is record
      Target : Store.Optional_Reference;
      Position : Events.Store.Cursor;
      Target_Location : Location_Type;
   end record;

   No_Element : constant Cursor :=
     (Target => Store.Null_Reference, Position => Events.Store.No_Element,
      Target_Location => None);
end Yaml.Events.Context;
