private with Ada.Text_IO;
private with Ada.Directories;

private package Yada.Lexing is
   pragma Preelaborate;

   type Lexer is private;

   function From_File (File_Name : String) return Lexer;
   function From_String (S: String) return Lexer;

private
   type Lexer is record
      Input    : Ada.Text_IO.File_Type;
      Input_Length, Input_At : Ada.Directories.File_Size;
      Sentinel : Integer;
      Pos      : Integer;
      Buffer   : not null access String;
   end record;
end Yada.Lexing;