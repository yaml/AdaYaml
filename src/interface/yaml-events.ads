--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Yaml.Events is
   --  parent of several event-holding collection types

   --  raised when trying to manipulate an event holder object while a
   --  Stream_Instance based on that object exists.
   State_Error : exception;
private
   type Event_Array is array (Positive range <>) of aliased Event;
   type Event_Array_Access is access Event_Array;

   type Event_Holder is abstract limited new Refcount_Base with record
      Length : Natural := 0;
      Data : not null Event_Array_Access := new Event_Array (1 .. 256);
   end record;

   overriding procedure Finalize (Object : in out Event_Holder);

   procedure Copy_Data (Source : Event_Holder;
                        Target : not null Event_Array_Access)
     with Pre => Target.all'Length >= Source.Data.all'Length;

   procedure Grow (Object : in out Event_Holder'Class);
end Yaml.Events;
