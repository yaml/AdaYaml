--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text.Pool;
with Yaml.Events.Store;
private with Yaml.Events.Context;

package Yaml.Transformator.Annotation_Processor is
   type Instance (<>) is limited new Transformator.Instance with private;

   function New_Processor
     (Pool : Text.Pool.Reference;
      Externals : Events.Store.Reference := Events.Store.New_Store)
      return Pointer;

   overriding procedure Put (Object : in out Instance; E : Event);

   function Has_Next (Object : Instance) return Boolean;

   function Next (Object : in out Instance) return Event;
private
   procedure Append (Object : in out Instance; E : Event; Start : Natural);

   type Annotated_Node is record
      Impl : Transformator.Pointer;
      Depth : Natural;
   end record;

   type Node_Array is array (Positive range <>) of Annotated_Node;
   type Node_Array_Pointer is access Node_Array;

   type Current_State_Type is (Existing, Existing_But_Held_Back,
                               Swallowing_Document_End, Localizing_Alias,
                               Absent);

   type Instance is limited new Transformator.Instance with record
      Context : Events.Context.Instance;
      Pool : Text.Pool.Reference;
      Depth, Count, Stream_Depth : Natural := 0;
      Current, Held_Back : Event;
      Current_State : Current_State_Type := Absent;
      Current_Stream : Events.Store.Optional_Stream_Reference;
      Annotations : not null Node_Array_Pointer := new Node_Array (1 .. 16);
   end record;

   overriding procedure Finalize (Object : in out Instance);
end Yaml.Transformator.Annotation_Processor;
