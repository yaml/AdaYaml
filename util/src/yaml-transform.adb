--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Parser;
with Yaml.Presenter;
with Yaml.Destination.Text_IO;
with Yaml.Source;

procedure Yaml.Transform is
   Input : Source.Pointer;
   P  : Parser.Reference;
   Pres  : Presenter.Instance;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := Source.File.As_Source (Ada.Command_Line.Argument (1));
   end if;

   P.Set_Input (Input);
   Pres.Set_Output (Destination.Text_IO.As_Destination (Ada.Text_IO.Standard_Output));

   Pres.Put (P);
end Yaml.Transform;
