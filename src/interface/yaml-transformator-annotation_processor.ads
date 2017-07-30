--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Yaml.Events.Queue;

package Yaml.Transformator.Annotation_Processor is
   type Instance is limited new Transformator.Instance with private;

   procedure Put (Object : in out Instance; E : Event);

   function Has_Next (Object : Instance) return Boolean;

   function Next (Object : in out Instance) return Event;
private
   type Instance is limited new Transformator.Instance with record
      Buffer : Events.Queue.Instance;
   end record;
end Yaml.Transformator.Annotation_Processor;
