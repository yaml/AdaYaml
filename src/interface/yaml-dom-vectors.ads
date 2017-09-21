--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Indefinite_Vectors;

package Yaml.Dom.Vectors is new Ada.Containers.Indefinite_Vectors
  (Positive, Document_Reference);
