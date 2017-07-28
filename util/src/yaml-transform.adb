--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Stream_Concept;
with Yaml.Parser.Stream;
with Yaml.Presenter;
with Yaml.Destination.Text_IO;
with Yaml.Source;
with Yaml.Transformator.Canonical;
with Yaml.Transformation;

procedure Yaml.Transform is
   package Parser_Transformation is new Transformation (Parser.Stream);
   package Transformation_Stream is new Stream_Concept
     (Parser_Transformation.Instance, Parser_Transformation.Next);

   Input : Source.Pointer;
   P  : Parser.Reference := Parser.New_Parser;
   Trans : Parser_Transformation.Instance := Parser_Transformation.Transform (P.Data);
   Pres  : Presenter.Instance;

   procedure Consume_Canonical is new
     Presenter.Consume (Transformation_Stream);
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := Source.File.As_Source (Ada.Command_Line.Argument (1));
   end if;

   P.Set_Input (Input);
   Trans.Append (new Transformator.Canonical.Instance);
   Pres.Set_Output (Destination.Text_IO.As_Destination (Ada.Text_IO.Standard_Output));

   Consume_Canonical (Pres, Trans);
end Yaml.Transform;
