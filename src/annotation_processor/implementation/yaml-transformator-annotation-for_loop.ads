--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Yaml.Events.Store;

package Yaml.Transformator.Annotation.For_Loop is
   type Instance is limited new Transformator.Instance with private;

   overriding procedure Put (Object : in out Instance; E : Event);

   overriding function Has_Next (Object : Instance) return Boolean;

   overriding function Next (Object : in out Instance) return Event;

   function New_For_Loop (Pool : Text.Pool.Reference;
                          Node_Context : Node_Context_Type;
                          Processor_Context : Events.Context.Reference;
                          Swallows_Previous : out Boolean)
                          return not null Pointer;
private
   type State_Type is not null access procedure (Object : in out Instance;
                                                 E : Event);

   procedure Initial (Object : in out Instance; E : Event);
   procedure After_Annotation_Start (Object : in out Instance; E : Event);
   procedure After_Variable_Name (Object : in out Instance; E : Event);
   procedure In_Sequence_Parameter (Object : in out Instance; E : Event);
   procedure After_Sequence_Parameter (Object : in out Instance; E : Event);
   procedure After_Annotation_End (Object : in out Instance; E : Event);
   procedure In_Body (Object : in out Instance; E : Event);
   procedure Emitting (Object : in out Instance; E : Event);

   type Emitting_State_Type is (Emit_Sequence_Start, Emit_Sequence_Body_Start,
                                Emit_Sequence_Body, Emit_Sequence_End,
                                Emitting_Finished);

   type Instance is limited new Transformator.Instance with record
      Node_Properties : Properties;
      Context : Events.Context.Reference;
      Depth : Natural := 0;
      State : State_Type := Initial'Access;
      Emitting_State : Emitting_State_Type := Emit_Sequence_Start;
      Loop_Variable : Events.Context.Symbol_Cursor;
      Loop_Variable_Store, Body_Store : Events.Store.Optional_Reference;
      Loop_Variable_Target, Body_Start, Next_Event :
        Events.Store.Element_Cursor;
      Header_Locals, Body_Locals : Events.Context.Local_Scope_Cursor;
   end record;

   overriding procedure Finalize (Object : in out Instance);
end Yaml.Transformator.Annotation.For_Loop;
