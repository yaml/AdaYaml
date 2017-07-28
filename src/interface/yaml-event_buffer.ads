--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Finalization;

package Yaml.Event_Buffer is
   type Instance is new Stream_Base with private;
   type Reference (Data : not null access Instance) is tagged private with
     Implicit_Dereference => Data;

   procedure Append (Object : in out Instance; E : Event);

   function Next (Object : in out Instance) return Event;
   procedure Reset (Object : in out Instance);
private
   type Event_Array is array (Positive range <>) of Event;
   type Event_Array_Access is access Event_Array;

   type Reference (Data : not null access Instance) is new
     Ada.Finalization.Controlled with null record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   type Instance is new Stream_Base with record
      Iterator : Positive := 1;
      Last     : Natural := 0;
      Data     : not null Event_Array_Access := new Event_Array (1 .. 256);
   end record;
   type Instance_Pointer is access all Instance;
end Yaml.Event_Buffer;
