--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Yaml.Events.Store;

package Yaml.Transformator.Annotation.Inject is
   type Instance is limited new Transformator.Instance with private;

   overriding procedure Put (Object : in out Instance; E : Event);

   overriding function Has_Next (Object : Instance) return Boolean;

   overriding function Next (Object : in out Instance) return Event;

   function New_Inject (Pool : Text.Pool.Reference;
                        Node_Context : Node_Context_Type;
                        Processor_Context : Events.Context.Reference;
                        Swallows_Previous : out Boolean)
                        return not null Pointer;
private
   type State_Type is not null access procedure (Object : in out Instance;
                                                 E : Event);

   procedure Initial (Object : in out Instance; E : Event);
   procedure After_Annotation_Start (Object : in out Instance; E : Event);
   procedure After_Annotation_End (Object : in out Instance; E : Event);
   procedure Injecting (Object : in out Instance; E : Event);
   procedure Injecting_Aliased (Object : in out Instance; E : Event);
   procedure After_Inject_End (Object : in out Instance; E : Event);

   type Instance is limited new Transformator.Instance with record
      Context : Events.Context.Reference;
      Injecting_Mapping : Boolean;
      Depth : Natural;
      State : State_Type;
      Current : Event;
      Current_Exists : Boolean;
      Current_Aliased : Events.Store.Optional_Stream_Reference;
   end record;
end Yaml.Transformator.Annotation.Inject;
