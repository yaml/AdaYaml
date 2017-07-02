--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Directories;
with Ada.Direct_IO;

package body Utils is
   function File_Content (Path : String) return String is
      File_Size : constant Natural := Natural (Ada.Directories.Size (Path));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
   begin
      File_String_IO.Open (File, File_String_IO.In_File, Path);
      return Contents : File_String do
         File_String_IO.Read  (File, Contents);
         File_String_IO.Close (File);
      end return;
   end File_Content;
end Utils;
