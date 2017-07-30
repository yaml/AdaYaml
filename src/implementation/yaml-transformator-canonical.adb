--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text;
with Yaml.Tags;

package body Yaml.Transformator.Canonical is
   procedure Put (Object : in out Instance; E : Event) is
      use type Text.Reference;

      function Is_Non_Specific (Tag : Text.Reference) return Boolean is
         (Tag = Text.Empty or Tag = Tags.Exclamation_Mark);
   begin
      if Object.Current_Exists then
         raise Constraint_Error with
           "Must retrieve current event before inputting another one";
      end if;
      Object.Current := E;
      Object.Current_Exists := True;

      case Object.Current.Kind is
         when Document_Start =>
            Object.Current.Implicit_Start := False;
         when Mapping_Start =>
            if Is_Non_Specific (Object.Current.Collection_Properties.Tag) then
               Object.Current.Collection_Properties.Tag := Tags.Mapping;
            end if;
            Object.Current.Collection_Style := Flow;
         when Sequence_Start =>
            if Is_Non_Specific (Object.Current.Collection_Properties.Tag) then
               Object.Current.Collection_Properties.Tag := Tags.Sequence;
            end if;
            Object.Current.Collection_Style := Flow;
         when Scalar =>
            if Is_Non_Specific (Object.Current.Scalar_Properties.Tag) then
               Object.Current.Scalar_Properties.Tag := Tags.String; --  TODO
            end if;
            Object.Current.Scalar_Style := Double_Quoted;
         when others => null;
      end case;
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_Exists);

   function Next (Object : in out Instance) return Event is
   begin
      if Object.Current_Exists then
         Object.Current_Exists := False;
         return Object.Current;
      else
         raise Constraint_Error with "No next event!";
      end if;
   end Next;
end Yaml.Transformator.Canonical;
