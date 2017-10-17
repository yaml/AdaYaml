--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Strings_Edit.UTF8;

package Lexer.Base.Unicode is
   --  this package assumes that the input is encoded in UTF-8.

   subtype Rune is Strings_Edit.UTF8.UTF8_Code_Point;

   function Next (Object : in out Instance'Class) return Rune with Inline;

   End_Of_Input    : constant Rune := 4;
   Line_Feed       : constant Rune := 10;
   Carriage_Return : constant Rune := 13;
end Lexer.Base.Unicode;
