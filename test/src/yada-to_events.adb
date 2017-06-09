with Ada.Text_IO;
with Ada.Command_Line;

with Yada.Sources.Text_IO; use Yada.Sources.Text_IO;
with Yada.Sources.Files;   use Yada.Sources.Files;
with Yada.Parsing; use Yada.Parsing;
with Yada.Streams; use Yada.Streams;
with Yada.Events;  use Yada.Events;
with Yada.Sources;  use Yada.Sources;

procedure Yada.To_Events is
   Input : Source_Access;
   Yaml  : Parser;
   Cur   : Events.Event;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := As_Source (Ada.Command_Line.Argument (1));
   end if;

   Parse (Yaml, Input);
   loop
      Cur := Next (Yaml);
      Ada.Text_IO.Put_Line (To_String (Cur));
      exit when Cur.Kind = Stream_End;
   end loop;
end Yada.To_Events;
