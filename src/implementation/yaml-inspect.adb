--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Exceptions;
with Yaml.Events.Queue;
with Yaml.Parser;
With GNAT.Strings;
procedure Yaml.Inspect (Input : String) is
   use GNAT.Strings;
   type Error_Kind is (None, From_Lexer, From_Parser);

   P : Parser.Instance;
   Cur_Pos     : Positive := 1;
   Next_Pos    : Positive;
   Cur_Event   : Event;
   Read_Events : constant Events.Queue.Reference := Events.Queue.New_Queue;
   Occurred_Error : Error_Kind := None;
   Lexer_Token_Start, Lexer_Token_End : Mark;
   Exception_Message : String_Access;
begin
   P.Set_Input (Input);
   Start_Emitting;
   Start_Parsed_Input;
   begin
      loop
         Cur_Event := P.Next;
         Read_Events.Value.Append (Cur_Event);

         if Cur_Event.Start_Position.Index > Cur_Pos then
            Next_Pos := Cur_Pos;
            while Next_Pos <= Input'Last and then
              Next_Pos < Cur_Event.Start_Position.Index loop
               if Input (Next_Pos) = '#' then
                  if Cur_Pos < Next_Pos then
                     Emit_Whitespace (Input (Cur_Pos .. Next_Pos - 1));
                     Cur_Pos := Next_Pos;
                  end if;
                  while Next_Pos < Cur_Event.Start_Position.Index and then
                    Input (Next_Pos) /= Character'Val (10) loop
                     Next_Pos := Next_Pos + 1;
                  end loop;
                  Emit_Comment (Input (Cur_Pos .. Next_Pos - 1));
               end if;
               Next_Pos := Next_Pos + 1;
            end loop;
            if Cur_Pos < Next_Pos then
               Emit_Whitespace (Input (Cur_Pos .. Next_Pos - 1));
               Cur_Pos := Next_Pos;
            end if;
         end if;
         Start_Rendered_Event (Cur_Event);
         declare
            Content : constant String :=
              Input (Cur_Pos .. Cur_Event.End_Position.Index - 1);
            Cur : Positive := Content'First;
            Start : Positive;
         begin
            while Cur <= Content'Last loop
               if Content (Cur) in ' ' | Character'Val (10) then
                  Start := Cur;
                  loop
                     Cur := Cur + 1;
                     exit when Cur > Content'Last or else
                       Content (Cur) in ' ' | Character'Val (10);
                  end loop;
                  Emit_Whitespace (Content (Start .. Cur - 1));
                  exit when Cur > Content'Last;
               end if;
               Start := Cur;
               case Content (Cur) is
                  when '&' =>
                     loop
                        Cur := Cur + 1;
                        exit when Cur > Content'Last or else
                          Content (Cur) in ' ' | Character'Val (10);
                     end loop;
                     Emit_Anchor (Content (Start .. Cur - 1));
                  when '!' =>
                     loop
                        Cur := Cur + 1;
                        exit when Cur > Content'Last or else
                          Content (Cur) in ' ' | Character'Val (10);
                     end loop;
                     Emit_Tag (Content (Start .. Cur - 1));
                  when others =>
                     Emit_Event_Content (Content (Start .. Content'Last));
                     exit;
               end case;
            end loop;
         end;
         End_Rendered_Event;
         Cur_Pos := Cur_Event.End_Position.Index;
         exit when Cur_Event.Kind = Stream_End;
      end loop;
   exception
      when Error : Lexer_Error =>
         Emit_Unparseable (Input (Cur_Pos .. Input'Last));
         Occurred_Error := From_Lexer;
         Lexer_Token_Start := P.Current_Lexer_Token_Start;
         Lexer_Token_End := P.Current_Input_Character;
         Exception_Message := new String'(Ada.Exceptions.Exception_Message (Error));
      when Error : Parser_Error =>
         Emit_Unparseable (Input (Cur_Pos .. Input'Last));
         Occurred_Error := From_Parser;
         Lexer_Token_Start := P.Recent_Lexer_Token_Start;
         Lexer_Token_End := P.Recent_Lexer_Token_End;
         Exception_Message := new String'(Ada.Exceptions.Exception_Message (Error));
   end;
   End_Parsed_Input;
   Start_Parsed_Output;
   case Occurred_Error is
      when None =>
         declare
            Iterator : constant Events.Queue.Stream_Reference :=
              Events.Queue.As_Stream (Read_Events);
         begin
            loop
               Cur_Event := Iterator.Value.Next;
               Emit_Raw_Event (Cur_Event);
               exit when Cur_Event.Kind = Stream_End;
            end loop;
         end;
      when From_Lexer =>
         Emit_Lexer_Error (Lexer_Token_Start, Lexer_Token_End,
                           Exception_Message.all);
      when From_Parser =>
         Emit_Parser_Error (Lexer_Token_Start, Lexer_Token_End,
                            Exception_Message.all);
   end case;
   End_Parsed_Output;
   Finish_Emitting;
end Yaml.Inspect;
