with System;

package Yaml.Destinations.C_Strings is
   type C_String_Destination is new Destination with private;

   function As_Destination (Pointer : System.Address;
                            Size : Interfaces.C.size_t;
                            Size_Written : access Interfaces.C.size_t)
                            return Destination_Access;

   overriding procedure Write_Data (D : in out C_String_Destination;
                                    Buffer : String);
private
   type C_String_Destination is new Destination with record
      Pointer : System.Address;
      Size : Integer;
      Size_Written : access Interfaces.C.size_t;
   end record;
end Yaml.Destinations.C_Strings;
