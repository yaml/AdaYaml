--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Text.Pool;
with Yaml.Events.Context;

package Yaml.Transformator.Annotation is
   type Constructor is not null access
     function (Pool : Text.Pool.Reference;
               Context : Events.Context.Reference) return not null Pointer;

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Constructor, Ada.Strings.Hash, Standard."=");

   Map : Maps.Map;
end Yaml.Transformator.Annotation;
