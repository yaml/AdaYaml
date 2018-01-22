--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text.Pool;
with Yaml.Events.Store;
private with Yaml.Events.Context;
private with Yaml.Transformator.Annotation;

package Yaml.Transformator.Annotation_Processor is
   type Instance (<>) is limited new Transformator.Instance with private;

   function New_Processor
     (Pool : Text.Pool.Reference;
      Externals : Events.Store.Reference := Events.Store.New_Store)
      return Pointer;

   overriding procedure Put (Object : in out Instance; E : Event)
     with Pre => not Object.Has_Next;

   function Has_Next (Object : Instance) return Boolean;

   function Next (Object : in out Instance) return Event;
private
   procedure Append (Object : in out Instance; E : Event);

   type Annotated_Node is record
      Swallows_Next : Boolean;
      Impl : Transformator.Pointer;
      Depth : Natural;
   end record;

   type Node_Array is array (Positive range <>) of Annotated_Node;
   type Node_Array_Pointer is access Node_Array;

   type Level_Array is array (Positive range <>) of
     Annotation.Node_Context_Type;
   type Level_Array_Pointer is access Level_Array;

   type Current_State_Type is (Existing, Event_Held_Back, Releasing_Held_Back,
                               Swallowing_Document_End, Localizing_Alias,
                               Absent);

   type Next_Event_Storage_Type is (No, Searching, Finishing, Yes);

   type Instance is limited new Transformator.Instance with record
      Context : Events.Context.Reference;
      Pool : Text.Pool.Reference;
      Annotation_Count, Level_Count, Stream_Depth : Natural := 0;
      Current, Held_Back : Event;
      Current_State : Current_State_Type := Absent;
      Current_Stream : Events.Store.Optional_Stream_Reference;
      Annotations : not null Node_Array_Pointer := new Node_Array (1 .. 16);
      Levels : not null Level_Array_Pointer := new Level_Array (1 .. 64);
      May_Finish_Transformation : Boolean := False;
      Next_Event_Storage : Next_Event_Storage_Type := No;
   end record;

   overriding procedure Finalize (Object : in out Instance);
end Yaml.Transformator.Annotation_Processor;
