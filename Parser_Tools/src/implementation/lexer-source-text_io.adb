--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Lexer.Source.Text_IO is
   procedure Read_Data (S : in out Instance; Buffer : out String;
                        Length : out Natural) is
   begin
      Length := Buffer'First;
      loop
         Ada.Text_IO.Get_Line (S.File_Pointer.all,
                               Buffer (Length .. Buffer'Last), Length);
         exit when Length = Buffer'Last;
         if Ada.Text_IO.End_Of_File (S.File_Pointer.all) then
            Length := Length + 1;
            Buffer (Length) := Character'Val (4);
            exit;
         end if;
         Buffer (Length + 1) := Character'Val (10);
         Length := Length + 2;
      end loop;
   end Read_Data;

   function As_Source (File : Ada.Text_IO.File_Access) return Pointer is
     (Pointer'(new Instance'(Source.Instance with File_Pointer => File)));
end Lexer.Source.Text_IO;
