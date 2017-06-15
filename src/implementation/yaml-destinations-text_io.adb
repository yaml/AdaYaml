package body Yaml.Destinations.Text_IO is
   function As_Destination (File : Ada.Text_IO.File_Access)
                            return Destination_Access is
     (new Text_Destination'(Ada.Finalization.Limited_Controlled with File => File));

   procedure Write_Data (D : in out Text_Destination; Buffer : String) is
   begin
      Ada.Text_IO.Put (D.File.all, Buffer);
   end Write_Data;
end Yaml.Destinations.Text_IO;
