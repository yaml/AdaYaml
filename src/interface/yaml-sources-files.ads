private with Ada.Streams.Stream_IO;
private with Ada.Directories;

package Yaml.Sources.Files is
   --  this provides streams which are backed by files on the file system.

   type File_Source is new Source with private;

   overriding procedure Read_Data (S : in out File_Source; Buffer : out String;
                                   Length : out Natural);

   overriding procedure Finalize (Object : in out File_Source);

   function As_Source (File_Path : String) return Source_Access;

private
   type File_Source is new Source with record
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
      Input_At, Input_Length : Ada.Directories.File_Size;
   end record;
end Yaml.Sources.Files;
