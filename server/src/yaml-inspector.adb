--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Exceptions;
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

   procedure Write_With_Properties (Content : String;
                                    Target : in out Unbounded_String;
                                    Kind : Events.Event_Kind) is
      Cur : Positive := Content'First;
      Start : Positive;
   begin
      Outer : while Cur <= Content'Last loop
         loop
            case Content (Cur) is
               when ' ' => Append (Target, ' ');
               when Character'Val (10) => Append (Target, "<br />");
               when others => exit;
            end case;
            Cur := Cur + 1;
            if Cur > Content'Last then
               exit Outer;
            end if;
         end loop;
         case Content (Cur) is
            when '!' =>
               Start := Cur;
               loop
                  Cur := Cur + 1;
                  exit when Cur > Content'Last or else
                    Content (Cur) in ' ' | Character'Val (10);
               end loop;
               Append (Target, "<span class=""tag"">");
               Write_Escaped (Content (Start .. Cur - 1), Target);
               Append (Target, "</span>");
            when '&' =>
               Start := Cur;
               loop
                  Cur := Cur + 1;
                  exit when Cur > Content'Last or else
                    Content (Cur) in ' ' | Character'Val (10);
               end loop;
               Append (Target, "<span class=""anchor"">");
               Write_Escaped (Content (Start .. Cur - 1), Target);
               Append (Target, "</span>");
            when others =>
               Append (Target, "<span class=""" & Kind'Img & """>");
               Write_Escaped (Content (Cur .. Content'Last), Target);
               Append (Target, "</span>");
               exit Outer;
         end case;
      end loop Outer;
   end Write_With_Properties;

   procedure Write_Wrapped (Content : String; Class_Index : Positive;
                            Kind : Events.Event_Kind;
                            Target : in out Unbounded_String) is
      use Ada.Strings.Fixed;
   begin
      Append (Target, "<span class=""event event" &
                Ada.Strings.Fixed.Trim (Class_Index'Img, Ada.Strings.Left) & """>");
      Write_With_Properties (Content, Target, Kind);
      Append (Target, "</span>");
   end Write_Wrapped;

   procedure Write_Result (Yaml_Output, Event_Output : Unbounded_String) is
      use Ada.Text_IO;
   begin
      Put_Line ("<table class=""yaml-inspect""><tr>");
      Put_Line ("  <td class=""yaml-input"">");
      Put ("    <pre><code>");
      Unbounded_IO.Put (Yaml_Output);
      Put_Line ("</code></pre>");
      Put_Line ("  </td><td class=""yaml-events"">");
      Put ("    <pre><code>");
      Unbounded_IO.Put (Event_Output);
      Put_Line ("</code></pre>");
      Put_Line ("  </td>");
      Put_Line ("</tr></table>");
   end Write_Result;

   procedure Write_Error_Name_And_Message (Error_Name, Message : String;
                                           Output : in out Unbounded_String) is
      Last_Written : Natural := 0;
      Cur : Positive := Message'First;
      Cur_Column : Positive := 1;
      Column_Start : Boolean := True;
   begin
      Append (Output, "<span class=""error-name"">");
      Append (Output, Error_Name);
      Append (Output, ":</span><br />  ");
      loop
         while Cur <= Message'Last and then Message (Cur) /= ' ' and then
           Cur_Column <= 60 loop
            Cur := Cur + 1;
            Cur_Column := Cur_Column + 1;
         end loop;
         if Cur < Message'Last then
            Write_Escaped (Message (Last_Written + 1 .. Message'Last), Output);
            exit;
         elsif Cur <= Message'Last and then Message (Cur) = ' ' then
            Write_Escaped (Message (Last_Written + 1 .. Cur), Output);
            Last_Written := Cur;
            Column_Start := False;
         elsif Column_Start then
            Write_Escaped (Message (Last_Written + 1 .. Cur - 1), Output);
            Append (Output, "<br />  ");
            Last_Written := Cur - 1;
            Cur_Column := 1;
         else
            Append (Output, "<br />  ");
            Cur_Column := 1;
            Column_Start := True;
         end if;
      end loop;
   end Write_Error_Name_And_Message;

   procedure Write_Error_Header (Error_Name, Message, Input : String;
                                 Output : in out Unbounded_String) is
   begin
      Append (Output, "<table class=""yaml-inspect""><tr>");
      Append (Output, Character'Val (10) & "  <td class=""yaml-input"">");
      Append (Output,
              Character'Val (10) & "    <pre><code>");
      Write_Escaped (Input, Output);
      Append (Output, "</code></pre>" & Character'Val (10) & "  </td><td class=""yaml-error"">");
      Append (Output, Character'Val (10) &
                      "    <pre><code>");
      Write_Error_Name_And_Message (Error_Name, Message, Output);
      Append (Output, "<br /><br />");
   end Write_Error_Header;

   procedure Write_Lexer_Error (Message, Input : String; Pos : Mark;
                                Highlight_Char : Positive) is
      Line_End : Positive;
      Output : Unbounded_String;
   begin
      Write_Error_Header ("Lexer_Error", Message, Input, Output);
      Write_Escaped (Input (Pos.Index + 1 - Pos.Column .. Pos.Index - 1),
                     Output);
      Append (Output, "<span class=""containing-token"">");
      Write_Escaped (Input (Pos.Index .. Highlight_Char - 1), Output);
      Append (Output, "</span><span class=""illegal-character"">");
      Write_Escaped (Input (Highlight_Char .. Highlight_Char), Output);
      Append (Output, "</span>");
      Line_End := Highlight_Char + 1;
      while Line_End <= Input'Last and then
        Input (Line_End) /= Character'Val (10) loop
         Line_End := Line_End + 1;
      end loop;
      Write_Escaped (Input (Highlight_Char + 1 .. Line_End - 1), Output);
      Append (Output, "</code></pre>" & Character'Val (10) & "  </td>");
      Append (Output, Character'Val (10) & "</tr></table>" & Character'Val (10));
      Ada.Text_IO.Unbounded_IO.Put (Output);
   end Write_Lexer_Error;

   procedure Write_Parser_Error (Message, Input : String;
                                 Start_Mark, End_Mark : Mark) is
      Line_End : Positive;
      Output : Unbounded_String := Null_Unbounded_String;
   begin
      Write_Error_Header ("Parser_Error", Message, Input, Output);
      Write_Escaped (
        Input (Start_Mark.Index + 1 - Start_Mark.Column .. Start_Mark.Index - 1),
        Output);
      Append (Output, "<span class=""illegal-token"">");
      Write_Escaped (Input (Start_Mark.Index .. End_Mark.Index - 1), Output);
      Append (Output, "</span>");
      Line_End := End_Mark.Index;
      while Line_End <= Input'Last and then
        Input (Line_End) /= Character'Val (10) loop
         Line_End := Line_End + 1;
      end loop;
      Write_Escaped (Input (End_Mark.Index .. Line_End - 1), Output);
      Append (Output, "</code></pre>" & Character'Val (10) & "  </td>");
      Append (Output, Character'Val (10) & "</tr></table>" & Character'Val (10));
      Ada.Text_IO.Unbounded_IO.Put (Output);
   end Write_Parser_Error;

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
      if Cur_Event.Start_Position.Index > Cur_Pos then
         Write_Escaped
           (Input (Cur_Pos .. Positive'Min (Input'Length, Cur_Event.Start_Position.Index - 1)),
            Yaml_Output);
         Cur_Pos := Cur_Event.Start_Position.Index;
      end if;
      Next_Pos := Cur_Event.End_Position.Index;
      Write_Wrapped (Input (Cur_Pos .. Next_Pos - 1), Event_Count,
                     Cur_Event.Kind, Yaml_Output);
      Write_Wrapped (Events.To_String (Cur_Event), Event_Count,
                     Cur_Event.Kind, Event_Output);
      Append (Event_Output, "<br />");
      Event_Count := Event_Count + 1;
      Cur_Pos := Next_Pos;
      exit when Cur_Event.Kind = Events.Stream_End;
   end loop;
   Write_Result (Yaml_Output, Event_Output);
exception
   when Error : Lexer_Error =>
      Write_Lexer_Error (Ada.Exceptions.Exception_Message (Error), Input,
                         P.Current_Lexer_Token_Start,
                         P.Current_Input_Character.Index);
   when Error : Parser_Error =>
      Write_Parser_Error (Ada.Exceptions.Exception_Message (Error), Input,
                          P.Recent_Lexer_Token_Start,
                          P.Recend_Lexer_Token_End);
end Yaml.Inspector;
