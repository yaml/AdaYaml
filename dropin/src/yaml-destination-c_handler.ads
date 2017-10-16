--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with System;
with Yaml.C;

package Yaml.Destination.C_Handler is
   type Instance is new Destination.Instance with private;

   function As_Destination (Handler : Yaml.C.Write_Handler;
                            Data : System.Address) return Pointer;

   overriding procedure Write_Data (Object : in out Instance; Buffer : String);
private
   type Instance is new Destination.Instance with record
      Handler : Yaml.C.Write_Handler;
      Data : System.Address;
   end record;
end Yaml.Destination.C_Handler;
