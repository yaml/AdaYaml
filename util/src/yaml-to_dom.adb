--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Dom.Loading;
with Yaml.Dom.Node;

pragma Unreferenced (Yaml.Dom.Node);

procedure Yaml.To_Dom is
   use type Dom.Node_Kind;

   Input : Source.Pointer;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Input := Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := Source.File.As_Source (Ada.Command_Line.Argument (1));
   end if;

   declare
      Document : constant Dom.Document_Reference :=
        Dom.Loading.From_Source (Input);
      Root_Node : constant not null access Dom.Node.Instance :=
        Document.Root.Value.Data;

      procedure Visit_Pair (Key, Value : not null access Dom.Node.Instance) is
      begin
         Ada.Text_IO.Put_Line ("Key: " & Key.Kind'Img);
         if Key.Kind = Dom.Scalar then
            Ada.Text_IO.Put_Line ("  """ & Key.Content.Value.Data.all & """");
         end if;
         Ada.Text_IO.Put_Line ("Value: " & Value.Kind'Img);
         if Value.Kind = Dom.Scalar then
            Ada.Text_IO.Put_Line ("  """ & Value.Content.Value.Data.all & """");
         end if;
      end Visit_Pair;
   begin
      Ada.Text_IO.Put_Line ("Root is " & Root_Node.Kind'Img);
      if Root_Node.Kind = Dom.Mapping then
         Root_Node.Pairs.Iterate (Visit_Pair'Access);
      end if;
   end;
end Yaml.To_Dom;
