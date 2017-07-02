--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;

package Yaml.Destinations.Text_IO is

   type Text_Destination is new Destination with private;
   
   function As_Destination (File : Ada.Text_IO.File_Access)
                            return Destination_Access
     with Pre => Ada.Text_IO.Is_Open (File.all);
   
   overriding procedure Write_Data (D : in out Text_Destination;
                                    Buffer : String);

private
   type Text_Destination is new Destination with record
      File : Ada.Text_IO.File_Access;
   end record;
end Yaml.Destinations.Text_IO;
