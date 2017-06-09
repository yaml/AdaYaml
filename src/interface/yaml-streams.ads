with Ada.Finalization;
with Yaml.Events;

package Yaml.Streams is
   Stream_Error : exception;

   --  has pointer semantics
   type Event_Stream is new Ada.Finalization.Controlled with private;

   overriding procedure Adjust (Stream : in out Event_Stream);
   overriding procedure Finalize (Stream : in out Event_Stream);

   function Next (Stream : in out Event_Stream'Class) return Events.Event;
   function Peek (Stream : in out Event_Stream'Class) return Events.Event;

   type Stream_Implementation is abstract tagged limited private;
   type Implementation_Access is access all Stream_Implementation'Class;

   procedure Fetch (Stream : in out Stream_Implementation;
                    E : out Events.Event) is abstract;

   --  override this if you have cleanup to do.
   procedure Close_Stream (Stream : in out Stream_Implementation) is null;

   procedure Create (Stream : in out Event_Stream'Class;
                     Implementation : Implementation_Access);
private
   type Event_Stream is new Ada.Finalization.Controlled with record
      Implementation : Implementation_Access;
   end record;

   type Stream_Implementation is abstract tagged limited record
      Refcount : Natural := 1;
      Cached : Events.Event;
      Peeked : Boolean := False;
   end record;
end Yaml.Streams;
