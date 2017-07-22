with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Yaml.Inspect;

package body Yaml.Servers is
   type Yaml_Params (Input_Length : Natural) is record
      Input : String (1 .. Input_Length);
   end record;

   function Params (Path : String) return Yaml_Params is
      Src_Pos : Positive := 1;
   begin
      while Src_Pos <= Path'Last and then Path (Src_Pos) /= '?' loop
         Src_Pos := Src_Pos + 1;
      end loop;
      Src_Pos := Src_Pos + 1;
      while Src_Pos <= Path'Last loop
         declare
            Name_End : Positive := Src_Pos;
         begin
            while Name_End <= Path'Last and then
              not (Path (Name_End) in '#' | '&' | '=') loop
               Name_End := Name_End + 1;
            end loop;
            if Name_End <= Path'Last and then Path (Name_End) = '=' then
               if Path (Src_Pos .. Name_End - 1) = "input" then
                  Src_Pos := Name_End + 1;
                  --while Src_Pos <= Path'Last and then
                  --  not (Path (Src_Pos) in '&' | '#') loop
                  while Src_Pos <= Path'Last loop
                    Src_Pos := Src_Pos + 1;
                  end loop;
                  return (Input_Length => Src_Pos - Name_End - 1,
                          Input => Path (Name_End + 1 .. Src_Pos - 1));
               else
                  Src_Pos := Name_End + 1;
                  while Src_Pos <= Path'Last and then
                    not (Path (Src_Pos) in '&' | '#') loop
                     Src_Pos := Src_Pos + 1;
                  end loop;
               end if;
            else
               Src_Pos := Name_End;
            end if;
         end;
         exit when Src_Pos > Path'Last or else Path (Src_Pos) /= '&';
         Src_Pos := Src_Pos + 1;
      end loop;
      return (Input_Length => 0, Input => "");
   end Params;

   procedure Generate (Client : in out Yaml_Client'Class;
                       Params : Yaml_Params) is
      Next_Event_Id : Positive;

      procedure Write_Output_Radio (Value : String) is
      begin
         Client.Accumulate_Body
           ("<label class=""output-style""><input type=""radio"" " &
              "name=""output-style"" value=""" & Value & """ />" & Value &
              "</label>");
      end Write_Output_Radio;

      procedure Start_Emitting is
      begin
         Client.Accumulate_Body
           ("<!doctype html><html lang=""en""><head><title>YAML 1.3 Widget</title>" &
              "<script src=""resources/yaml-inspector.js""></script>" &
              "<link rel=""stylesheet"" href=""resources/yaml.css"" />" &
              "</head><body><form id=""yaml-inspect-form""><table id=""yaml-inspect"">" &
              "<tr class=""header""><td colspan=""2""><button id=""edit"" type=""button"">Edit</button><button id=""cancel"" type=""button"">Cancel</button>");
         Write_Output_Radio ("Events");
         Write_Output_Radio ("Canonical");
         Write_Output_Radio ("Like Input");
         Write_Output_Radio ("JSON");
         Client.Accumulate_Body ("</td></tr>");
      end Start_Emitting;

      procedure Finish_Emitting is
      begin
         Client.Accumulate_Body ("</tr></table></form></body></html>");
      end Finish_Emitting;

      procedure Write_Escaped (Content : String) is
         First_New : Positive := Content'First;
      begin
         for Cur in Content'Range loop
            case Content (Cur) is
            when '<' =>
               if First_New < Cur then
                  Client.Accumulate_Body (Content (First_New .. Cur - 1));
               end if;
               First_New := Cur + 1;
               Client.Accumulate_Body ("&lt;");
            when '>' =>
               if First_New < Cur then
                  Client.Accumulate_Body (Content (First_New .. Cur - 1));
               end if;
               First_New := Cur + 1;
               Client.Accumulate_Body ("&lt;");
            when others => null;
            end case;
         end loop;
         if First_New <= Content'Last then
            Client.Accumulate_Body (Content (First_New .. Content'Last));
         end if;
      end Write_Escaped;

      procedure Start_Parsed_Input is
      begin
         Client.Accumulate_Body ("<tr><td id=""yaml-input""><textarea name=""input"" id=""editable-input"">");
         Write_Escaped (Params.Input);
         Client.Accumulate_Body ("</textarea><pre id=""rendered-input""><code>");
         Next_Event_Id := 1;
      end Start_Parsed_Input;

      procedure End_Parsed_Input is
      begin
         Client.Accumulate_Body ("</code></pre></td>");
      end End_Parsed_Input;

      procedure Start_Parsed_Output is
      begin
         Client.Accumulate_Body ("<td id=""yaml-output""><pre><code>");
         Next_Event_Id := 1;
      end Start_Parsed_Output;

      procedure End_Parsed_Output is
      begin
         Client.Accumulate_Body ("</td></tr>");
      end End_Parsed_Output;

      generic
         Span_Class : String;
      procedure Span_Emitter (Content : String);

      procedure Span_Emitter (Content : String) is
      begin
         Client.Accumulate_Body ("<span class=""" & Span_Class & """>");
         Write_Escaped (Content);
         Client.Accumulate_Body ("</span>");
      end Span_Emitter;

      procedure Emit_Whitespace is new Span_Emitter ("whitespace");
      procedure Emit_Comment is new Span_Emitter ("comment");
      procedure Emit_Tag is new Span_Emitter ("tag");
      procedure Emit_Anchor is new Span_Emitter ("anchor");
      procedure Emit_Event_Content is new Span_Emitter ("content");

      procedure Emit_Raw_Event (E : Event) is
      begin
         Client.Accumulate_Body
           ("<span class=""event event" & Ada.Strings.Fixed.Trim
              (Next_Event_Id'Img, Ada.Strings.Left) & ' ' & E.Kind'Img & """>");
         Client.Accumulate_Body (To_String (E));
         Client.Accumulate_Body ("</span>" & Character'Val (10));
         Next_Event_Id := Next_Event_Id + 1;
      end Emit_Raw_Event;

      procedure Start_Rendered_Event (E : Event) is
      begin
         Client.Accumulate_Body
           ("<span class=""event event" & Ada.Strings.Fixed.Trim
              (Next_Event_Id'Img, Ada.Strings.Left) & ' ' & E.Kind'Img & """>");
      end Start_Rendered_Event;

      procedure End_Rendered_Event is
      begin
         Client.Accumulate_Body ("</span>");
         Next_Event_Id := Next_Event_Id + 1;
      end End_Rendered_Event;

      procedure Emit_Unparseable is new Span_Emitter ("unparseable");

      procedure Write_Error_Name_And_Message (Error_Name, Message : String) is
         Last_Written : Natural := 0;
         Cur : Positive := Message'First;
         Cur_Column : Positive := 1;
         Column_Start : Boolean := True;
      begin
         Client.Accumulate_Body ("<span class=""error-name"">");
         Client.Accumulate_Body (Error_Name);
         Client.Accumulate_Body (":</span>" & Character'Val (10) & "  ");
         loop
            while Cur <= Message'Last and then Message (Cur) /= ' ' and then
              Cur_Column <= 60 loop
               Cur := Cur + 1;
               Cur_Column := Cur_Column + 1;
            end loop;
            if Cur < Message'Last then
               Write_Escaped (Message (Last_Written + 1 .. Message'Last));
               exit;
            elsif Cur <= Message'Last and then Message (Cur) = ' ' then
               Write_Escaped (Message (Last_Written + 1 .. Cur));
               Last_Written := Cur;
               Column_Start := False;
            elsif Column_Start then
               Write_Escaped (Message (Last_Written + 1 .. Cur - 1));
               Client.Accumulate_Body (Character'Val (10) & "  ");
               Last_Written := Cur - 1;
               Cur_Column := 1;
            else
               Client.Accumulate_Body (Character'Val (10) & "  ");
               Cur_Column := 1;
               Column_Start := True;
            end if;
         end loop;
         Client.Accumulate_Body (Character'Val (10) & Character'Val (10));
      end Write_Error_Name_And_Message;

      procedure Emit_Lexer_Error (Token_Start, Error_Char : Mark;
                                  Error_Message : String) is
         Line_End : Positive;
      begin
         Write_Error_Name_And_Message ("Lexer_Error", Error_Message);
         Write_Escaped (Params.Input (Token_Start.Index + 1 - Token_Start.Column .. Token_Start.Index - 1));
         Client.Accumulate_Body ("<span class=""containing-token"">");
         Write_Escaped (Params.Input (Token_Start.Index .. Error_Char.Index - 1));
         Client.Accumulate_Body ("</span><span class=""illegal-character"">");
         Write_Escaped (Params.Input (Error_Char.Index .. Error_Char.Index));
         Client.Accumulate_Body ("</span>");
         Line_End := Error_Char.Index + 1;
         while Line_End <= Params.Input'Last and then
           Params.Input (Line_End) /= Character'Val (10) loop
            Line_End := Line_End + 1;
         end loop;
         Write_Escaped (Params.Input (Error_Char.Index + 1 .. Line_End - 1));
      end Emit_Lexer_Error;

      procedure Emit_Parser_Error (Token_Start, Token_End : Mark;
                                   Error_Message : String) is
         Line_End : Positive;
      begin
         Write_Error_Name_And_Message ("Parser_Error", Error_Message);
         Write_Escaped (Params.Input (Token_Start.Index + 1 - Token_Start.Column .. Token_Start.Index - 1));
         Client.Accumulate_Body ("<span class=""illegal-token"">");
         Write_Escaped (Params.Input (Token_Start.Index .. Token_End.Index - 1));
         Client.Accumulate_Body ("</span>");
         Line_End := Token_End.Index;
         while Line_End <= Params.Input'Last and then
           Params.Input (Line_End) /= Character'Val (10) loop
            Line_End := Line_End + 1;
         end loop;
         Write_Escaped (Params.Input (Token_End.Index .. Line_End - 1));
      end Emit_Parser_Error;

      procedure Inspect_HTML is new Yaml.Inspect;
   begin
      Inspect_HTML (Params.Input);
   end Generate;

   procedure Do_Get (Client : in out Yaml_Client) is
      Status : HTTP.Status_Line renames Get_Status_Line (Client);
   begin
      Ada.Text_IO.Put_Line ("GET: " & Status.Kind'Img);
      case Status.Kind is
         when HTTP.None =>
            Client.Reply_Text (404, "Not found", "Resource not found");
         when HTTP.File =>
            Ada.Text_IO.Put_Line ("File: " & Status.File);
            Client.Send_Status_Line (200, "OK");
            Client.Send_Date;
            Client.Send_Server;
            Client.Send_Content_Type ("text/html");
            Generate (Client, Params (Status.File));
            Client.Send_Body;
         when HTTP.URI =>
            Ada.Text_IO.Put_Line ("URI: " & Status.Path);
            Ada.Text_IO.Put_Line ("Query: " & Status.Query);
            Client.Send_Status_Line (200, "OK");
            Client.Send_Date;
            Client.Send_Server;
            Client.Send_Content_Type ("text/html");
            Generate (Client, Params (Status.Path));
            Client.Send_Body;
      end case;
   exception when Error : others =>
         Ada.Text_IO.Put_Line ("Got exception: " & Ada.Exceptions.Exception_Name (Error));
         Ada.Text_IO.Put_Line ("Message: " & Ada.Exceptions.Exception_Message (Error));
         raise;
   end Do_Get;
end Yaml.Servers;
