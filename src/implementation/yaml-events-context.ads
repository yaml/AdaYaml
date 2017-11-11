--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Events.Store;

package Yaml.Events.Context is
   type Instance is private;
   type Cursor is private;

   type Location_Type is (Document, Stream, External, None);

   function Create (External : Store.Reference := Store.New_Store)
                    return Instance;

   function External_Store (Object : Instance) return Store.Reference;
   function Stream_Store (Object : Instance) return Store.Reference;
   function Document_Store (Object : Instance) return Store.Reference;

   function Position (Object : Instance; Alias : Text.Reference) return Cursor;
   function Location (Position : Cursor) return Location_Type;

   No_Element : constant Cursor;

   function Retrieve (Pos : Cursor) return Store.Stream_Reference
     with Pre => Pos /= No_Element;
private
   type Instance is new Ada.Finalization.Controlled with record
      Document_Ref, Stream_Ref, External_Ref : Store.Reference;
   end record;

   type Cursor is record
      Target : Store.Optional_Reference;
      Position : Events.Store.Anchored_Position;
      Target_Location : Location_Type;
   end record;

   No_Element : constant Cursor :=
     (Target => Store.Null_Reference, Position => Events.Store.No_Element,
      Target_Location => None);
end Yaml.Events.Context;
