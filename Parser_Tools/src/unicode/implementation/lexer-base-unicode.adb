--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Lexer.Base.Unicode is
   function Next (Object : in out Instance) return Rune is
   begin
      return Ret : Rune do
         Strings_Edit.UTF8.Get (Object.Buffer.all, Object.Pos, Ret);
      end return;
   end Next;
end Lexer.Base.Unicode;
