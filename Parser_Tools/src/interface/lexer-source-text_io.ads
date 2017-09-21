--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;

package Lexer.Source.Text_IO is
   --  this package provides a source type that is backed by Ada.Text_IO's file
   --  type. the primary use-case is to read from stdin. use Yaml.Sources.Files
   --  for standard files, it is more efficient.

   type Instance is new Source.Instance with private;

   overriding procedure Read_Data (S : in out Instance; Buffer : out String;
                                   Length : out Natural);

   function As_Source (File : Ada.Text_IO.File_Access) return Pointer
     with Pre => Ada.Text_IO.Is_Open (File.all);

private
   type Instance is new Source.Instance with record
      File_Pointer : Ada.Text_IO.File_Access;
   end record;
end Lexer.Source.Text_IO;
