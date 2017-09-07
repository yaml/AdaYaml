--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Finalization;

package Yaml.Events.Queue is
   type Instance is limited new Refcount_Base with private;
   type Reference is tagged private;
   type Accessor (Data : not null access Instance) is null record with
     Implicit_Dereference => Data;

   procedure Append (Object : in out Instance; E : Event);
   function Length (Object : in Instance) return Natural;
   function First (Object : in Instance) return Event;
   procedure Dequeue (Object : in out Instance);

   function Value (Object : Reference) return Accessor;
   function New_Queue return Reference;

   type Stream_Instance is new Refcount_Base with private;
   type Stream_Reference is tagged private;
   type Stream_Accessor (Data : not null access Stream_Instance) is
     null record with Implicit_Dereference => Data;

   function Value (Object : Stream_Reference) return Stream_Accessor;
   function Next (Object : in out Stream_Instance) return Event;

   package Iteration is
      --  must be in child package so that it is not a dispatching operation
      function As_Stream (Object : Reference) return Stream_Reference;
   end Iteration;
private
   type Reference is new Ada.Finalization.Controlled with record
      Data : not null access Instance;
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   type Instance is limited new Event_Holder with record
      First_Pos    : Positive := 1;
      Stream_Count : Natural := 0;
   end record;

   overriding procedure Copy_Data (Source : Instance;
                                   Target : not null Event_Array_Access)
     with Pre => Target.all'Length >= Source.Data.all'Length;

   type Instance_Pointer is access all Instance;

   type Stream_Instance is new Refcount_Base with record
      Buffer : Reference;
      Offset : Natural := 0;
   end record;

   overriding procedure Finalize (Object : in out Stream_Instance);

   type Stream_Reference is new Ada.Finalization.Controlled with record
      Data : not null access Stream_Instance;
   end record;

   overriding procedure Adjust (Object : in out Stream_Reference);
   overriding procedure Finalize (Object : in out Stream_Reference);

end Yaml.Events.Queue;
