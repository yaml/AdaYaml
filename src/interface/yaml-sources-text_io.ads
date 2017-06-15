with Ada.Text_IO;

package Yaml.Sources.Text_IO is
   --  this package provides a source type that is backed by Ada.Text_IO's file
   --  type. the primary use-case is to read from stdin. use Yaml.Sources.Files
   --  for standard files, it is more efficient.

   type Text_Source is new Source with private;

   overriding procedure Read_Data (S : in out Text_Source; Buffer : out String;
                                   Length : out Natural);

   function As_Source (File : Ada.Text_IO.File_Access) return Source_Access
     with Pre => Ada.Text_IO.Is_Open (File.all);

private
   type Text_Source is new Source with record
      File   : Ada.Text_IO.File_Access;
   end record;
end Yaml.Sources.Text_IO;
