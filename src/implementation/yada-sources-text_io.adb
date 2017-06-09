package body Yada.Sources.Text_IO is
   procedure Read_Data (S : in out Text_Source; Buffer : out String;
                        Length : out Natural) is
   begin
      Ada.Text_IO.Get_Line (S.File.all, Buffer, Length);
      if Length < Buffer'Length then
         Buffer (Buffer'First + Length) := Character'Val (10);
         Length := Length + 1;
      elsif Ada.Text_IO.End_Of_File (S.File.all) then
         Buffer (Buffer'First + Length) := Character'Val (4);
         Length := Length + 1;
      end if;
   end Read_Data;

   function As_Source (File : Ada.Text_IO.File_Access) return Source_Access is
     (Source_Access'(new Text_Source'(Source with File => File)));
end Yada.Sources.Text_IO;
