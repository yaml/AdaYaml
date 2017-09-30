--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Events is
   procedure Free is new Ada.Unchecked_Deallocation
     (Event_Array, Event_Array_Access);

   procedure Finalize (Object : in out Event_Holder) is
      Ptr : Event_Array_Access := Object.Data;
   begin
      Free (Ptr);
   end Finalize;

   procedure Copy_Data (Source : Event_Holder;
                        Target : not null Event_Array_Access) is
   begin
      Target (Target.all'First .. Target.all'First + Source.Data.all'Length - 1)
        := Source.Data.all;
   end Copy_Data;

   procedure Grow (Object : in out Event_Holder'Class) is
      New_Data : constant not null Event_Array_Access := new Event_Array
        (1 .. Object.Data.all'Length * 2);
      Old_Data : Event_Array_Access := Object.Data;
   begin
      Object.Copy_Data (New_Data); --  dispatches
      Object.Data := New_Data;
      Free (Old_Data);
   end Grow;
end Yaml.Events;
