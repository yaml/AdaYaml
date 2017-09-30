--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Sequence_Data;
with Yaml.Dom.Mapping_Data;

package Yaml.Dom.Node is
   type Instance (Kind : Node_Kind) is record
      Tag : Text.Reference;
      case Kind is
         when Scalar =>
            Scalar_Style : Scalar_Style_Type;
            Content : Text.Reference;
         when Sequence =>
            Sequence_Style : Collection_Style_Type;
            Items : Sequence_Data.Instance;
         when Mapping =>
            Mapping_Style : Collection_Style_Type;
            Pairs : Mapping_Data.Instance;
      end case;
   end record;

   function "=" (Left, Right : Instance) return Boolean;

   function Hash (Object : Instance) return Ada.Containers.Hash_Type;
end Yaml.Dom.Node;
