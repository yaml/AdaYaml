--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;

package Yaml.Destination.Text_IO is

   type Instance is new Destination.Instance with private;
   
   function As_Destination (File : Ada.Text_IO.File_Access) return Pointer
     with Pre => Ada.Text_IO.Is_Open (File.all);
   
   overriding procedure Write_Data (D : in out Instance; Buffer : String);

private
   type Instance is new Destination.Instance with record
      File_Pointer : Ada.Text_IO.File_Access;
   end record;
end Yaml.Destination.Text_IO;
