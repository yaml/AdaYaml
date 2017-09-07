--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text.Pool;
with Yaml.Events.Store;
private with Yaml.Events.Context;

package Yaml.Transformator.Annotation_Processor is
   type Instance (<>) is limited new Transformator.Instance with private;

   function New_Processor (Pool : Text.Pool.Reference) return Pointer;

   procedure Set_Globals (Object : in out Instance;
                          Value : Events.Store.Instance);

   overriding procedure Put (Object : in out Instance; E : Event);

   function Has_Next (Object : Instance) return Boolean;

   function Next (Object : in out Instance) return Event;
private
   procedure Append (Object : in out Instance; E : Event);

   type Annotated_Node is record
      Impl : Transformator.Pointer;
      Depth : Natural;
   end record;

   type Node_Array is array (Positive range <>) of Annotated_Node;
   type Node_Array_Pointer is access Node_Array;

   type Instance is limited new Transformator.Instance with record
      Context : Events.Context.Instance;
      Pool : Text.Pool.Reference;
      Depth, Count : Natural := 0;
      Current : Event;
      Current_Exists : Boolean := False;
      Annotations : not null Node_Array_Pointer := new Node_Array (1 .. 16);
   end record;

   overriding procedure Finalize (Object : in out Instance);
end Yaml.Transformator.Annotation_Processor;
