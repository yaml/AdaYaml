--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Yaml.Transformator.Annotation.Concatenation is
   type Instance is limited new Transformator.Instance with private;

   overriding procedure Put (Object : in out Instance; E : Event);

   overriding function Has_Next (Object : Instance) return Boolean;

   overriding function Next (Object : in out Instance) return Event;

   function New_Concatenation (Pool : Text.Pool.Reference)
                               return not null Pointer;
private
   type State_Type is not null access procedure (Object : in out Instance'Class;
                                                 E : Event);

   procedure Initial (Object : in out Instance'Class; E : Event);
   procedure After_Annotation_Start (Object : in out Instance'Class; E : Event);
   procedure After_Annotation_End (Object : in out Instance'Class; E : Event);
   procedure After_List_Start (Object : in out Instance'Class; E : Event);
   procedure In_Sequence (Object : in out Instance'Class; E : Event);
   procedure After_Sequence (Object : in out Instance'Class; E : Event);
   procedure After_String (Object : in out Instance'Class; E : Event);
   procedure After_List_End (Object : in out Instance'Class; E : Event);

   type Instance is limited new Transformator.Instance with record
      Pool : Text.Pool.Reference;
      Depth : Natural := 0;
      State : State_Type := Initial'Access;
      Current_Exists : Boolean := False;
      Current : Event;
   end record;
end Yaml.Transformator.Annotation.Concatenation;
