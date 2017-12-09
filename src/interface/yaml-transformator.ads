--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;

package Yaml.Transformator is
   type Instance is abstract tagged limited private;
   type Pointer is access Instance'Class;

   procedure Put (Object : in out Instance; E : Event) is abstract;

   function Has_Next (Object : Instance) return Boolean is abstract;

   function Next (Object : in out Instance) return Event is abstract;
private
   type Instance is abstract limited new Ada.Finalization.Limited_Controlled
     with null record;
end Yaml.Transformator;
