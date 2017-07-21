--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;
with Yaml.Events;

package Yaml.Stream is
   --  event streams are the heart of YAML processing. a parser generates an
   --  event stream, while a presenter consumes it. serializing data structures
   --  creates an event stream, a deserializer consumes it. an event stream
   --  generator should extend Event_Stream and Stream_Implementation.

   Stream_Error : exception;

   --  smart pointer with pointer semantics. you may override Initialize if
   --  you want to declare a type that is automatically initialized.
   type Reference is new Ada.Finalization.Controlled with private;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   --  queries the next event from the stream.
   function Next (S : in out Reference'Class) return Events.Event;

   --  returns the next event that would be returned by Next, but without
   --  advancing the stream (i.e. this function is idempotent). useful for
   --  semantic processing of the stream.
   function Peek (S : in out Reference'Class) return Events.Event;

   --  holder of implementation values. when implementing a stream generator,
   --  put the generator's state in a type derived from this.
   type Implementation is abstract tagged limited private;
   type Implementation_Pointer is access all Implementation'Class;

   --  to be provided by the stream generator implementation
   procedure Fetch (S : in out Implementation;
                    E : out Events.Event) is abstract;

   --  override this if you have cleanup to do.
   procedure Close_Stream (S : in out Implementation) is null;

   --  the generator implementation should call this to sets its implementation
   --  object to be the backend of its Event_Stream-derived interface type.
   procedure Create (S : in out Reference'Class;
                     Impl : Implementation_Pointer);

   --  accessor for the stream's implementation. mainly useful for adding
   --  subroutines to the type deriving from Event_Stream.
   function Implementation_Access (S : Reference'Class)
                                   return Implementation_Pointer;
private
   type Reference is new Ada.Finalization.Controlled with record
      Impl : Implementation_Pointer;
   end record;

   type Implementation is abstract tagged limited record
      Refcount : Natural := 1;
      Cached : Events.Event;
      Peeked : Boolean := False;
   end record;
end Yaml.Stream;
