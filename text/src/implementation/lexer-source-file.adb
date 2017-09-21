--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Lexer.Source.File is

   overriding procedure Read_Data (S : in out Instance; Buffer : out String;
                                   Length : out Natural) is
      use type Ada.Directories.File_Size;
   begin
      Length := Natural'Min (Buffer'Length, Natural (S.Input_Length - S.Input_At));
      declare
         subtype Read_Blob is String (1 .. Length);
      begin
         Read_Blob'Read (S.Stream, Buffer (Buffer'First .. Buffer'First + Length - 1));
         S.Input_At := S.Input_At + Ada.Directories.File_Size (Length);
      end;
   end Read_Data;

   overriding procedure Finalize (Object : in out Instance) is
   begin
      Ada.Streams.Stream_IO.Close (Object.File);
   end Finalize;

   function As_Source (File_Path : String) return Pointer is
      Ret : constant access Instance :=
        new Instance'(Source.Instance with Input_At => 0,
                         Input_Length => Ada.Directories.Size (File_Path),
                         others => <>);
   begin
      Ada.Streams.Stream_IO.Open (Ret.File, Ada.Streams.Stream_IO.In_File,
                                  File_Path);
      Ret.Stream := Ada.Streams.Stream_IO.Stream (Ret.File);
      return Pointer (Ret);
   end As_Source;
end Lexer.Source.File;
