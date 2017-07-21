--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Parser;
with Yaml.Stream;
with Yaml.Events;  use Yaml.Events;

procedure Yaml.To_Events is
   Input : Source.Pointer;
   P  : Parser.Reference;
   Cur   : Events.Event;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := Source.File.As_Source (Ada.Command_Line.Argument (1));
   end if;

   P.Set_Input (Input);
   loop
      Cur := P.Next;
      Ada.Text_IO.Put_Line (To_String (Cur));
      exit when Cur.Kind = Stream_End;
   end loop;
end Yaml.To_Events;
