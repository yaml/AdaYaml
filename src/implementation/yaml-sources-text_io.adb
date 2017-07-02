--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Sources.Text_IO is
   procedure Read_Data (S : in out Text_Source; Buffer : out String;
                        Length : out Natural) is
   begin
      Length := Buffer'First;
      loop
         Ada.Text_IO.Get_Line (S.File.all, Buffer (Length .. Buffer'Last),
                               Length);
         exit when Length = Buffer'Last;
         if Ada.Text_IO.End_Of_File (S.File.all) then
            Length := Length + 1;
            Buffer (Length) := Character'Val (4);
            exit;
         end if;
         Buffer (Length + 1) := Character'Val (10);
         Length := Length + 2;
      end loop;
   end Read_Data;

   function As_Source (File : Ada.Text_IO.File_Access) return Source_Access is
     (Source_Access'(new Text_Source'(Source with File => File)));
end Yaml.Sources.Text_IO;
