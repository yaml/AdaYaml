--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Yaml.Events;
with Yaml.Parsing;

procedure Yaml.Inspector is
   use Ada.Strings.Unbounded;
   use type Events.Event_Kind;

   function Yaml_From_Stdin return String is
      Stdin : constant Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Input;
      Buffer : Unbounded_String;
   begin
      while not Ada.Text_IO.End_Of_File (Stdin) loop
         Append (Buffer, Ada.Text_IO.Get_Line (Stdin));
      end loop;
      return To_String (Buffer);
   end Yaml_From_Stdin;

   function Yaml_From_File return String is
      File_Name : constant String  := Ada.Command_Line.Argument (1);
      File_Size : constant Natural := Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      return Contents : File_String do
         File_String_IO.Read  (File, Item => Contents);
         File_String_IO.Close (File);
      end return;
   end Yaml_From_File;

   procedure Write_Escaped (Content : String;
                            Target : in out Unbounded_String) is
   begin
      for C of Content loop
         case C is
            when '<' => Append (Target, "&lt;");
            when '>' => Append (Target, "&gt;");
            when Character'Val (10) => Append (Target, "<br />");
            when others => Append (Target, C);
         end case;
      end loop;
   end Write_Escaped;

   procedure Write_Wrapped (Content : String; Class_Index : Positive;
                            Target : in out Unbounded_String) is
      use Ada.Strings.Fixed;
   begin
      Append (Target, "<span class=""event event" &
                Ada.Strings.Fixed.Trim (Class_Index'Img, Ada.Strings.Left) & """>");
      Write_Escaped (Content, Target);
      Append (Target, "</span>");
   end Write_Wrapped;

   procedure Write_Result (Yaml_Output, Event_Output : Unbounded_String) is
      use Ada.Text_IO;
   begin
      Put_Line ("<table class=""yaml-inspect""><tr>");
      Put_Line ("  <td>");
      Put ("    <pre><code class=""yaml-input"">");
      Unbounded_IO.Put (Yaml_Output);
      Put_Line ("</code></pre>");
      Put_Line ("  </td><td>");
      Put ("    <pre><code class=""yaml-events"">");
      Unbounded_IO.Put (Event_Output);
      Put_Line ("</code></pre>");
      Put_Line ("  </td>");
      Put_Line ("</tr></table>");
   end Write_Result;

   Input : constant String :=
     (if Ada.Command_Line.Argument_Count = 0 then
         Yaml_From_Stdin else Yaml_From_File);
   P : Parsing.Parser;

   Cur_Pos   : Positive := 1;
   Next_Pos  : Positive;
   Cur_Event : Events.Event;
   Event_Count : Positive := 1;

   Yaml_Output, Event_Output : Unbounded_String;
begin
   P.Set_Input (Input);
   loop
      Cur_Event := P.Next;
      if Positive (Cur_Event.Start_Position.Index) > Cur_Pos then
         Write_Escaped
           (Input (Cur_Pos .. Positive'Min (Input'Length, Positive (Cur_Event.Start_Position.Index) - 1)),
            Yaml_Output);
         Cur_Pos := Positive (Cur_Event.Start_Position.Index);
      end if;
      Next_Pos := Positive (Cur_Event.End_Position.Index);
      Write_Wrapped (Input (Cur_Pos .. Next_Pos - 1), Event_Count, Yaml_Output);
      Write_Wrapped (Events.To_String (Cur_Event), Event_Count, Event_Output);
      Append (Event_Output, "<br />");
      Event_Count := Event_Count + 1;
      Cur_Pos := Next_Pos;
      exit when Cur_Event.Kind = Events.Stream_End;
   end loop;
   Write_Result (Yaml_Output, Event_Output);
end Yaml.Inspector;
