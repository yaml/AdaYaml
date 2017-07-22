--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;

package Yaml.Source is
   --  a Source is anything that provides a YAML character stream. Sources are
   --  always single-use objects; the parser takes ownership of
   --  sources and deallocates them.

   type Instance is abstract new Ada.Finalization.Limited_Controlled with
     null record;
   type Pointer is access all Instance'Class;

   procedure Read_Data (S : in out Instance; Buffer : out String;
                        Length : out Natural) is abstract;
end Yaml.Source;
