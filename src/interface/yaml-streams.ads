--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;
with Yaml.Events;

package Yaml.Streams is
   --  event streams are the heart of YAML processing. a parser generates an
   --  event stream, while a presenter consumes it. serializing data structures
   --  creates an event stream, a deserializer consumes it. an event stream
   --  generator should extend Event_Stream and Stream_Implementation.

   Stream_Error : exception;

   --  smart pointer with pointer semantics. you may override Initialize if
   --  you want to declare a type that is automatically initialized.
   type Event_Stream is new Ada.Finalization.Controlled with private;

   overriding procedure Adjust (Stream : in out Event_Stream);
   overriding procedure Finalize (Stream : in out Event_Stream);

   --  queries the next event from the stream.
   function Next (Stream : in out Event_Stream'Class) return Events.Event;

   --  returns the next event that would be returned by Next, but without
   --  advancing the stream (i.e. this function is idempotent). useful for
   --  semantic processing of the stream.
   function Peek (Stream : in out Event_Stream'Class) return Events.Event;

   --  holder of implementation values. when implementing a stream generator,
   --  put the generator's state in a type derived from this.
   type Stream_Implementation is abstract tagged limited private;
   type Implementation_Access is access all Stream_Implementation'Class;

   --  to be provided by the stream generator implementation
   procedure Fetch (Stream : in out Stream_Implementation;
                    E : out Events.Event) is abstract;

   --  override this if you have cleanup to do.
   procedure Close_Stream (Stream : in out Stream_Implementation) is null;

   --  the generator implementation should call this to sets its implementation
   --  object to be the backend of its Event_Stream-derived interface type.
   procedure Create (Stream : in out Event_Stream'Class;
                     Implementation : Implementation_Access);

   --  accessor for the stream's implementation. mainly useful for adding
   --  subroutines to the type deriving from Event_Stream.
   function Implementation (Stream : Event_Stream'Class)
                            return Implementation_Access;
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
