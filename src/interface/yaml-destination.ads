--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;

package Yaml.Destination is

   type Instance is abstract new Ada.Finalization.Limited_Controlled with
     null record; 
   type Pointer is access all Instance'Class;
   
   procedure Write_Data (D : in out Instance; Buffer : String) is abstract;
end Yaml.Destination;
