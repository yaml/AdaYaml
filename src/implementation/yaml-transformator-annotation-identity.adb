--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Transformator.Annotation.Identity is
   procedure Put (Object : in out Instance; E : Event) is
   begin
      if Object.Current_Exists then
         raise Constraint_Error with
           "must remove event before inputting another one";
      end if;
      Object.Current_Exists := True;
      Object.Current := E;
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_Exists);

   function Next (Object : in out Instance) return Event is
   begin
      if not Object.Current_Exists then
         raise Constraint_Error with "no event to retrieve";
      end if;
      Object.Current_Exists := False;
      return Object.Current;
   end Next;

   function New_Identity return not null Pointer is
     (new Instance);
end Yaml.Transformator.Annotation.Identity;
