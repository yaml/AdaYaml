--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Parser;
with Yaml.Transformator.Annotation_Processor;

procedure Yaml.To_Events is
   use type Source.Pointer;

   Input : Source.Pointer;
   P     : Parser.Instance;
   Cur   : Event;

   Process_Annotations : Boolean := False;
begin
   for Cur_Argument in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Argument : constant String := Ada.Command_Line.Argument (Cur_Argument);
      begin
         if Argument'Length > 0 then
            if Argument (Argument'First) = '-' then
               if Argument = "-a" or Argument = "--process-annotations" then
                  Process_Annotations := True;
               else
                  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                        "Unknown option: " & Argument);
                  return;
               end if;
            elsif Input /= null then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "Too many files given!");
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            else
               Input := Source.File.As_Source (Argument);
            end if;
         end if;
      end;
   end loop;
   if Input = null then
      Input := Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   end if;
   P.Set_Input (Input);

   if Process_Annotations then
      declare
         Processor : constant Yaml.Transformator.Pointer
           := Yaml.Transformator.Annotation_Processor.New_Processor
             (P.Pool);
      begin
         loop
            Cur := P.Next;
            Processor.Put (Cur);
            while Processor.Has_Next loop
               Ada.Text_IO.Put_Line (To_String (Processor.Next));
            end loop;
            exit when Cur.Kind = Stream_End;
         end loop;
      end;
   else
      loop
         Cur := P.Next;
         Ada.Text_IO.Put_Line (To_String (Cur));
         exit when Cur.Kind = Stream_End;
      end loop;
   end if;

end Yaml.To_Events;
