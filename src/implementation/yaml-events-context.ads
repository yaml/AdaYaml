--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Events.Store;

package Yaml.Events.Context is
   type Instance is private;
   type Cursor is private;

   function Empty return Instance;

   function Global (Object : Instance) return Store.Reference;
   function Local (Object : Instance) return Store.Reference;

   function Position (Object : Instance; Alias : Text.Reference) return Cursor;

   No_Element : constant Cursor;

   function Retrieve (Pos : Cursor) return Store.Stream_Reference
     with Pre => Pos /= No_Element;
private
   type Instance is new Ada.Finalization.Controlled with record
      Local_Store, Global_Store : Store.Reference;
   end record;

   type Cursor is record
      Target : Store.Optional_Reference;
      Position : Events.Store.Anchored_Position;
   end record;

   No_Element : constant Cursor :=
     (Target => Store.Null_Reference, Position => Events.Store.No_Element);
end Yaml.Events.Context;
