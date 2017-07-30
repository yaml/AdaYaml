--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Transformator.Annotation_Processor is
   procedure Put (Object : in out Instance; E : Event) is
   begin
      --  TODO
      Object.Buffer.Append (E);
   end Put;

   function Has_Next (Object : Instance) return Boolean is
      (Object.Buffer.Length > 0);

   function Next (Object : in out Instance) return Event is
   begin
      return E : constant Event := Object.Buffer.First do
         Object.Buffer.Dequeue;
      end return;
   end Next;
end Yaml.Transformator.Annotation_Processor;
