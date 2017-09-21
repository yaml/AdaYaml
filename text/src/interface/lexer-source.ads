--  part of LexerBase, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;

package Lexer.Source is
   pragma Preelaborate;

   --  a Source is anything that provides a character stream. Sources are always
   --  single-use objects; the lexer takes ownership of sources and deallocates
   --  them.

   type Instance is abstract new Ada.Finalization.Limited_Controlled with
     null record;
   type Pointer is access all Instance'Class;

   procedure Read_Data (S : in out Instance; Buffer : out String;
                        Length : out Natural) is abstract;
end Lexer.Source;
