--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Stream;

package Yaml.Event_Buffer is
   type Reference is new Stream.Reference with private;

   procedure Append (Object : in out Reference; E : Event);
private
   type Event_Array is array (Positive range <>) of Event;
   type Event_Array_Access is access Event_Array;

   overriding procedure Initialize (Object : in out Reference);

   type Implementation is new Stream.Implementation with
      record
         Iterator : Positive := 1;
         Last     : Natural := 0;
         Data     : not null Event_Array_Access := new Event_Array (1 .. 256);
      end record;
   type Implementation_Pointer is access all Implementation;

   overriding procedure Fetch (Stream : in out Implementation;
                               E : out Event);

   overriding
   procedure Close_Stream (Stream : in out Implementation);

   type Reference is new Stream.Reference with null record;
end Yaml.Event_Buffer;
