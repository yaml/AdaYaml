--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Sources.Text_IO; use Yaml.Sources.Text_IO;
with Yaml.Sources.Files;   use Yaml.Sources.Files;
with Yaml.Parsing; use Yaml.Parsing;
with Yaml.Presenting; use Yaml.Presenting;
with Yaml.Destinations.Text_IO; use Yaml.Destinations.Text_IO;
with Yaml.Sources;  use Yaml.Sources;

procedure Yaml.Transform is
   Input : Source_Access;
   Yaml  : Parser;
   Pres  : Presenting.Presenter;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := As_Source (Ada.Command_Line.Argument (1));
   end if;

   Yaml.Set_Input (Input);
   Pres.Set_Output (As_Destination (Ada.Text_IO.Standard_Output));

   Pres.Put (Yaml);
end Yaml.Transform;
