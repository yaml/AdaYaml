--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Destination.Text_IO is
   function As_Destination (File : Ada.Text_IO.File_Access)
                            return Pointer is
     (new Instance'(Ada.Finalization.Limited_Controlled with
                    File_Pointer => File));

   procedure Write_Data (D : in out Instance; Buffer : String) is
   begin
      Ada.Text_IO.Put (D.File_Pointer.all, Buffer);
   end Write_Data;
end Yaml.Destination.Text_IO;
