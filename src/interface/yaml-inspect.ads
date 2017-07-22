--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

generic
   with procedure Start_Emitting is <>;
   with procedure Finish_Emitting is <>;
   with procedure Start_Parsed_Input is <>;
   with procedure End_Parsed_Input is <>;
   with procedure Start_Parsed_Output is <>;
   with procedure End_Parsed_Output is <>;
   with procedure Start_Rendered_Event (E : Event) is <>;
   with procedure End_Rendered_Event is <>;
   with procedure Emit_Whitespace (Content : String) is <>;
   with procedure Emit_Comment (Content : String) is <>;
   with procedure Emit_Anchor (Content : String) is <>;
   with procedure Emit_Tag (Content : String) is <>;
   with procedure Emit_Event_Content (Content : String) is <>;
   with procedure Emit_Raw_Event (E : Event) is <>;
   with procedure Emit_Unparseable (Content : String) is <>;
   with procedure Emit_Lexer_Error (Token_Start, Error_Char : Mark;
                                    Error_Message : String) is <>;
   with procedure Emit_Parser_Error (Token_Start, Token_End : Mark;
                                     Error_Message : String) is <>;
procedure Yaml.Inspect (Input : String);
