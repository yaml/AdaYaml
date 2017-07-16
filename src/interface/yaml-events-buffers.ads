with Yaml.Streams;

package Yaml.Events.Buffers is
   type Event_Buffer is new Yaml.Streams.Event_Stream with private;

   procedure Append (Buffer : in out Event_Buffer; E : Event);
private
   type Event_Array is array (Positive range <>) of Event;
   type Event_Array_Access is access Event_Array;

   overriding procedure Initialize (Buffer : in out Event_Buffer);

   type Event_Buffer_Implementation is new Streams.Stream_Implementation with
      record
         Iterator : Positive := 1;
         Last     : Natural := 0;
         Data     : not null Event_Array_Access := new Event_Array (1 .. 256);
      end record;
   type Event_Buffer_Implementation_Access is
     access all Event_Buffer_Implementation;

   overriding procedure Fetch (Stream : in out Event_Buffer_Implementation;
                               E : out Events.Event);

   overriding
   procedure Close_Stream (Stream : in out Event_Buffer_Implementation);

   type Event_Buffer is new Yaml.Streams.Event_Stream with null record;
end Yaml.Events.Buffers;
