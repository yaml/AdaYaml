--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Text.Pool;
with Yaml.Events.Context;

package Yaml.Transformator.Annotation is
   type Node_Context_Type is (Document_Root, Sequence_Item, Mapping_Key,
                              Mapping_Value, Parameter_Item);

   --  constructs an instance of an annotation transformator. parameters:
   --    Pool: the text pool to use for creating new scalar events.
   --    Node_Context: describes the surroundings of the annotation occurrence
   --                  inside the event stream.
   --    Processor_Context: current alias targets.
   --    Swallowes_Previous: may only be True in two cases:
   --      1. Node_Context is Document_Root. in this case, the previous
   --         Document_Start as well as the succeeding Document_End event is
   --         swallowed.
   --      2. Node_Context is Mapping_Value. in this case, the previous scalar
   --         mapping key is swallowed. if the previous mapping key was not a
   --         scalar, an Annotation_Error will be raised by the annotation
   --         processor.
   type Constructor is not null access
     function (Pool : Text.Pool.Reference; Node_Context : Node_Context_Type;
               Processor_Context : Events.Context.Reference;
               Swallows_Previous : out Boolean)
              return not null Pointer;

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Constructor, Ada.Strings.Hash, Standard."=");

   Map : Maps.Map;
end Yaml.Transformator.Annotation;
