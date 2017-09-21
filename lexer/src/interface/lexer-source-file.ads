--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Streams.Stream_IO;
private with Ada.Directories;

package Lexer.Source.File is
   --  this provides streams which are backed by files on the file system.

   type Instance is new Source.Instance with private;

   overriding procedure Read_Data (S : in out Instance; Buffer : out String;
                                   Length : out Natural);

   overriding procedure Finalize (Object : in out Instance);

   function As_Source (File_Path : String) return Pointer;

private
   type Instance is new Source.Instance with record
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
      Input_At, Input_Length : Ada.Directories.File_Size;
   end record;
end Lexer.Source.File;
