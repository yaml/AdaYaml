--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Interfaces.C;
with System;

package Yaml.Destination.C_String is
   type Instance is new Destination.Instance with private;

   function As_Destination (Raw : System.Address;
                            Size : Interfaces.C.size_t;
                            Size_Written : access Interfaces.C.size_t)
                            return Pointer;

   overriding procedure Write_Data (D : in out Instance;
                                    Buffer : String);
private
   type Instance is new Destination.Instance with record
      Raw : System.Address;
      Size : Integer;
      Size_Written : access Interfaces.C.size_t;
   end record;
end Yaml.Destination.C_String;
