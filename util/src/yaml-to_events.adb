--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Sources.Text_IO; use Yaml.Sources.Text_IO;
with Yaml.Sources.Files;   use Yaml.Sources.Files;
with Yaml.Parsing; use Yaml.Parsing;
with Yaml.Streams; use Yaml.Streams;
with Yaml.Events;  use Yaml.Events;
with Yaml.Sources;  use Yaml.Sources;

procedure Yaml.To_Events is
   Input : Source_Access;
   Yaml  : Parser;
   Cur   : Events.Event;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := As_Source (Ada.Command_Line.Argument (1));
   end if;

   Yaml.Set_Input (Input);
   loop
      Cur := Next (Yaml);
      Ada.Text_IO.Put_Line (To_String (Cur));
      exit when Cur.Kind = Stream_End;
   end loop;
end Yaml.To_Events;
