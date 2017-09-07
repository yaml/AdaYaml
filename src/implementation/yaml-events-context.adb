--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Context is
   function Empty return Instance is
     ((Ada.Finalization.Controlled with
       Local_Store => Store.New_Store, Global_Store => Store.New_Store));

   function Global (Object : Instance) return Store.Reference is
     (Object.Global_Store);

   function Local (Object : Instance) return Store.Reference is
     (Object.Local_Store);

   function Position (Object : Instance; Alias : Text.Reference) return Cursor
   is
      use type Store.Anchored_Position;
      Pos : Store.Anchored_Position :=
        Object.Local_Store.Value.Position (Alias);
   begin
      if Pos = Store.No_Element then
         Pos := Object.Global_Store.Value.Position (Alias);
         if Pos = Store.No_Element then
            return No_Element;
         else
            return (Target => Object.Global_Store.Optional, Position => Pos);
         end if;
      else
         return (Target => Object.Local_Store.Optional, Position => Pos);
      end if;
   end Position;

   function Retrieve (Pos : Cursor) return  Store.Stream_Reference is
     (Store.Iteration.Retrieve (Pos.Target.Required, Pos.Position));
end Yaml.Events.Context;
