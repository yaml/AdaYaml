--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Context is
   function Create (External : Store.Reference := Store.New_Store)
                    return Reference is
     ((Ada.Finalization.Controlled with Data => new Instance'(Refcount_Base with
       Document_Data => Store.New_Store, Stream_Data => Store.New_Store,
       External_Data => External)));

   function External_Store (Object : Reference) return Store.Accessor is
     (Object.Data.External_Data.Value);

   function Stream_Store (Object : Reference) return Store.Accessor is
     (Object.Data.Stream_Data.Value);

   function Document_Store (Object : Reference) return Store.Accessor is
     (Object.Data.Document_Data.Value);

   function Position (Object : Reference; Alias : Text.Reference) return Cursor
   is
      use type Store.Cursor;
      Pos : Store.Cursor :=
        Object.Data.Document_Data.Value.Find (Alias);
   begin
      if Pos = Store.No_Element then
         Pos := Object.Data.Stream_Data.Value.Find (Alias);
         if Pos = Store.No_Element then
            Pos := Object.Data.External_Data.Value.Find (Alias);
            if Pos = Store.No_Element then
               return No_Element;
            else
               return (Target => Object.Data.External_Data.Optional,
                       Position => Pos, Target_Location => External);
            end if;
         else
            return (Target => Object.Data.Stream_Data.Optional,
                    Position => Pos, Target_Location => Stream);
         end if;
      else
         return (Target => Object.Data.Document_Data.Optional,
                 Position => Pos, Target_Location => Document);
      end if;
   end Position;

   function Location (Position : Cursor) return Location_Type is
     (Position.Target_Location);

   function Retrieve (Pos : Cursor) return  Store.Stream_Reference is
     (Store.Iteration.Retrieve (Pos.Target.Required, Pos.Position));

   function First (Pos : Cursor) return Event is
     (Store.First (Pos.Target.Required, Pos.Position));

   procedure Adjust (Object : in out Reference) is
   begin
      Object.Data.Increase_Refcount;
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Object.Data.Decrease_Refcount;
   end Finalize;

   function Exists_In_Ouput (Position : Cursor) return Boolean is
     (Store.Exists_In_Output (Position.Position));

   procedure Set_Exists_In_Output (Position : in out Cursor) is
   begin
      Store.Set_Exists_In_Output (Position.Target.Value, Position.Position);
   end Set_Exists_In_Output;
end Yaml.Events.Context;
