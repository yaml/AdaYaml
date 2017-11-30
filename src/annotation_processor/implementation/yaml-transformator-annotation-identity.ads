--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Yaml.Transformator.Annotation.Identity is
   type Instance is limited new Transformator.Instance with private;

   overriding procedure Put (Object : in out Instance; E : Event);

   overriding function Has_Next (Object : Instance) return Boolean;

   overriding function Next (Object : in out Instance) return Event;

   function New_Identity return not null Pointer;
private
   type Instance is limited new Transformator.Instance with record
      Current_Exists : Boolean := False;
      Current : Event;
   end record;
end Yaml.Transformator.Annotation.Identity;
