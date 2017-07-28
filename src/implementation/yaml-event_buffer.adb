--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Event_Buffer is
   procedure Adjust (Object : in out Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;

   procedure Append (Object : in out Instance; E : Event) is
   begin
      if Object.Last = Object.Data.all'Last then
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (Event_Array, Event_Array_Access);
            New_Data : constant not null access Event_Array :=
              new Event_Array (1 .. Object.Data.all'Last * 2);
            Old_Data : Event_Array_Access := Object.Data;
         begin
            New_Data (1 .. Object.Data.all'Last) := Object.Data.all;
            Object.Data.all := New_Data.all;
            Free (Old_Data);
         end;
      end if;
      Object.Last := Object.Last + 1;
      Object.Data (Object.Last) := E;
   end Append;

   function Next (Object : in out Instance) return Event is
   begin
      if Object.Iterator > Object.Last then
         raise Constraint_Error with
           "Tried to query more events than in buffer";
      else
         return E : constant Event := Object.Data (Object.Iterator) do
            Object.Iterator := Object.Iterator + 1;
         end return;
      end if;
   end Next;

   procedure Reset (Object : in out Instance) is
   begin
      Object.Iterator := 1;
   end Reset;
end Yaml.Event_Buffer;
