--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Yaml.Events.Queue;

package Yaml.Transformator.Annotation.Vars is
   type Instance is limited new Transformator.Instance with private;

   overriding procedure Put (Object : in out Instance; E : Event);

   overriding function Has_Next (Object : Instance) return Boolean;

   overriding function Next (Object : in out Instance) return Event;

   function New_Vars
     (Pool :     Text.Pool.Reference; Node_Context : Node_Context_Type;
      Processor_Context :     Events.Context.Reference;
      Swallows_Previous : out Boolean) return not null Pointer;
private
   type State_Type is not null access procedure
     (Object : in out Instance; E : Event);

   procedure Initial (Object : in out Instance; E : Event);
   procedure After_Annotation_Start (Object : in out Instance; E : Event);
   procedure After_Annotation_End (Object : in out Instance; E : Event);
   procedure At_Mapping_Level (Object : in out Instance; E : Event);
   procedure Inside_Value (Object : in out Instance; E : Event);
   procedure After_Mapping_End (Object : in out Instance; E : Event);

   type Instance is limited new Transformator.Instance with record
      Context   : Events.Context.Reference;
      Depth     : Natural    := 0;
      State     : State_Type := Initial'Access;
      Cur_Queue : Events.Queue.Instance;
      Cur_Name  : Text.Reference;
   end record;
end Yaml.Transformator.Annotation.Vars;
