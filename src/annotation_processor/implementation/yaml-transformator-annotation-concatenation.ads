--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Text.Builder;
private with Yaml.Events.Store;

package Yaml.Transformator.Annotation.Concatenation is
   type Instance is limited new Transformator.Instance with private;

   overriding procedure Put (Object : in out Instance; E : Event);

   overriding function Has_Next (Object : Instance) return Boolean;

   overriding function Next (Object : in out Instance) return Event;

   function New_Concatenation (Pool : Text.Pool.Reference;
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
   procedure After_List_Start (Object : in out Instance; E : Event);
   procedure In_Sequence (Object : in out Instance; E : Event);
   procedure After_Sequence (Object : in out Instance; E : Event);
   procedure After_String (Object : in out Instance; E : Event);
   procedure After_List_End (Object : in out Instance; E : Event);

   type Builder_Pointer is access Text.Builder.Reference;

   type Instance is limited new Transformator.Instance with record
      Node_Properties : Properties;
      Builder : Builder_Pointer;
      Context : Events.Context.Reference;
      Pool : Text.Pool.Reference;
      Depth : Natural := 0;
      State : State_Type := Initial'Access;
      Current_Exists : Boolean := False;
      Current : Event;
      Current_Aliased : Events.Store.Optional_Stream_Reference;
   end record;
end Yaml.Transformator.Annotation.Concatenation;
