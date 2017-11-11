--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Context is
   function Create (External : Store.Reference := Store.New_Store)
                    return Instance is
     ((Ada.Finalization.Controlled with
       Document_Ref => Store.New_Store, Stream_Ref => Store.New_Store,
       External_Ref => External));

   function External_Store (Object : Instance) return Store.Reference is
     (Object.External_Ref);

   function Stream_Store (Object : Instance) return Store.Reference is
     (Object.Stream_Ref);

   function Document_Store (Object : Instance) return Store.Reference is
     (Object.Document_Ref);

   function Position (Object : Instance; Alias : Text.Reference) return Cursor
   is
      use type Store.Anchored_Position;
      Pos : Store.Anchored_Position :=
        Object.Document_Ref.Value.Position (Alias);
   begin
      if Pos = Store.No_Element then
         Pos := Object.Stream_Ref.Value.Position (Alias);
         if Pos = Store.No_Element then
            Pos := Object.External_Ref.Value.Position (Alias);
            if Pos = Store.No_Element then
               return No_Element;
            else
               return (Target => Object.External_Ref.Optional, Position => Pos,
                       Target_Location => External);
            end if;
         else
            return (Target => Object.Stream_Ref.Optional, Position => Pos,
                    Target_Location => Stream);
         end if;
      else
         return (Target => Object.Document_Ref.Optional, Position => Pos,
                 Target_Location => Document);
      end if;
   end Position;

   function Location (Position : Cursor) return Location_Type is
     (Position.Target_Location);

   function Retrieve (Pos : Cursor) return  Store.Stream_Reference is
     (Store.Iteration.Retrieve (Pos.Target.Required, Pos.Position));
end Yaml.Events.Context;
