with Ada.Text_IO;

package Yaml.Sources.Text_IO is
   type Text_Source is new Source with private;

   overriding procedure Read_Data (S : in out Text_Source; Buffer : out String;
                                   Length : out Natural);

   function As_Source (File : Ada.Text_IO.File_Access) return Source_Access;

private
   type Text_Source is new Source with record
      File   : Ada.Text_IO.File_Access;
   end record;
end Yaml.Sources.Text_IO;
