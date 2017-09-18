--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Sequence_Data;
with Yaml.Dom.Mapping_Data;

package Yaml.Dom.Node is
   type Instance (Kind : Node_Kind) is record
      Tag : Text.Reference;
      case Kind is
         when Scalar => Content : Text.Reference;
         when Sequence => Items : Sequence_Data.Instance;
         when Mapping => Pairs : Mapping_Data.Instance;
      end case;
   end record;

   function "=" (Left, Right : Instance) return Boolean;

   function Hash (Object : Instance) return Ada.Containers.Hash_Type;
end Yaml.Dom.Node;
