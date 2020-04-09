--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Finalization;

package Yaml.Events.Queue is
   type Instance is limited new Refcount_Base with private;
   type Instance_Access is access all Instance;
   type Reference is tagged private;
   type Accessor (Data : not null access Instance) is limited null record with
     Implicit_Dereference => Data;
   type Element_Accessor (Data : not null access Event) is limited null record
     with Implicit_Dereference => Data;

   type Mark is private;

   procedure Append (Object : in out Instance; E : Event);
   procedure Append (Object : in out Instance; E : Event; Position : out Mark);
   function Length (Object : Instance) return Natural;
   function Length (Object : Reference) return Natural;
   function First (Object : in Instance) return Event;
   procedure Dequeue (Object : in out Instance);

   function Value (Object : Reference) return Accessor;
   function New_Queue return Reference;

   function Element (Object : Instance; Position : Mark)
                     return Element_Accessor;

   type Stream_Instance is new Refcount_Base with private;
   type Stream_Instance_Access is access all Stream_Instance;
   type Stream_Reference is tagged private;
   type Stream_Accessor (Data : not null access Stream_Instance) is limited
     null record with Implicit_Dereference => Data;

   type Optional_Stream_Reference is tagged private;

   Null_Reference : constant Optional_Stream_Reference;

   function Value (Object : Stream_Reference) return Stream_Accessor;
   function Value (Object : Optional_Stream_Reference) return Stream_Accessor
     with Pre => Object /= Null_Reference;
   function Next (Object : in out Stream_Instance) return Event;

   function Required (Object : Optional_Stream_Reference'Class)
                      return Stream_Reference;
   function Optional (Object : Stream_Reference'Class)
                     return Optional_Stream_Reference;

   function As_Stream (Object : Reference'Class) return Stream_Reference;
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

   type Mark is new Positive;

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

   type Optional_Stream_Reference is new Ada.Finalization.Controlled with record
      Data : access Stream_Instance;
   end record;

   overriding procedure Adjust (Object : in out Optional_Stream_Reference);
   overriding procedure Finalize (Object : in out Optional_Stream_Reference);

   Null_Reference : constant Optional_Stream_Reference :=
     (Ada.Finalization.Controlled with Data => null);
end Yaml.Events.Queue;
