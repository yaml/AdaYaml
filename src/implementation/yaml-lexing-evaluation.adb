--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Yaml.Lexing.Evaluation is
   -----------------------------------------------------------------------------
   --  constant UTF-8 strings that may be generated from escape sequences
   -----------------------------------------------------------------------------

   function Next_Line return Ada.Strings.UTF_Encoding.UTF_8_String is
     (Ada.Strings.UTF_Encoding.Strings.Encode ("" & Character'Val (16#85#)));
   function Non_Breaking_Space return Ada.Strings.UTF_Encoding.UTF_8_String is
     (Ada.Strings.UTF_Encoding.Strings.Encode ("" & Character'Val (16#A0#)));
   function Line_Separator return Ada.Strings.UTF_Encoding.UTF_8_String is
     (Ada.Strings.UTF_Encoding.Wide_Strings.Encode
       ("" & Wide_Character'Val (16#2028#)));
   function Paragraph_Separator return Ada.Strings.UTF_Encoding.UTF_8_String is
     (Ada.Strings.UTF_Encoding.Wide_Strings.Encode
       ("" & Wide_Character'Val (16#2029#)));

   -----------------------------------------------------------------------------
   --  buffer for generating scalars
   -----------------------------------------------------------------------------

   type Out_Buffer_Type (Length : Positive) is record
      Content : UTF_8_String (1 .. Length);
      Pos : Positive := 1;
   end record;

   procedure Add (O : in out Out_Buffer_Type; C : Character) with Inline is
   begin
      O.Content (O.Pos) := C;
      O.Pos := O.Pos + 1;
   end Add;

   procedure Add (O : in out Out_Buffer_Type; S : String) with Inline is
   begin
      O.Content (O.Pos .. O.Pos + S'Length - 1) := S;
      O.Pos := O.Pos + S'Length;
   end Add;

   function New_Content_From (Pool : in out Strings.String_Pool;
                              O : Out_Buffer_Type) return Strings.Content is
      (Strings.From_String (Pool, O.Content (1 .. O.Pos - 1))) with Inline;

   -----------------------------------------------------------------------------
   --  implementation
   -----------------------------------------------------------------------------

   procedure Read_Plain_Scalar (L : in out Lexer; T : out Token) is
      --  our scalar cannot possibly have more content than the size of our
      --  buffer. Therefore, we read its value into a string of the same size
      --  so we never have to do any bounds checking and growing of the string.
      Result : Out_Buffer_Type (L.Buffer.all'Length);
      After_Newline_State : constant Lexer_State :=
        (if L.Flow_Depth = 0 then Line_Indentation'Access
           else Flow_Line_Indentation'Access);
   begin
      L.Seen_Multiline := False;
      Start_Token (L);
      if L.Proposed_Indentation /= -1 then
         L.Indentation := L.Proposed_Indentation;
         L.Proposed_Indentation := -1;
      end if;
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => <>,
            Kind => Plain_Scalar);
      Multiline_Loop : loop
         Inline_Loop : loop
            Add (Result, L.Cur);
            L.Cur := Next (L);
            case L.Cur is
               when ' ' =>
                  T.End_Pos := Cur_Mark (L);
                  declare
                     Space_Start : constant Positive := L.Pos;
                  begin
                     Space_Loop : loop
                        L.Cur := Next (L);
                        case L.Cur is
                           when Line_Feed | Carriage_Return =>
                              exit Inline_Loop;
                           when End_Of_Input =>
                              L.State := Stream_End'Access;
                              exit Multiline_Loop;
                           when '#' =>
                              L.State := Expect_Line_End'Access;
                              exit Multiline_Loop;
                           when ':' =>
                              if not Next_Is_Plain_Safe (L) then
                                 L.State := Inside_Line'Access;
                                 exit Multiline_Loop;
                              end if;
                              exit Space_Loop;
                           when Flow_Indicator =>
                              if L.Flow_Depth > 0 then
                                 L.State := Inside_Line'Access;
                                 exit Multiline_Loop;
                              end if;
                              exit Space_Loop;
                           when ' ' => null;
                           when others => exit Space_Loop;
                        end case;
                     end loop Space_Loop;
                     Add (Result, (1 .. L.Pos - Space_Start => ' '));
                  end;
               when ':' =>
                  if not Next_Is_Plain_Safe (L) then
                     T.End_Pos := Cur_Mark (L);
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when Flow_Indicator =>
                  if L.Flow_Depth > 0 then
                     T.End_Pos := Cur_Mark (L);
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when Line_Feed | Carriage_Return =>
                  T.End_Pos := Cur_Mark (L);
                  exit Inline_Loop;
               when End_Of_Input =>
                  if L.Pos /= L.Line_Start then
                     T.End_Pos := Cur_Mark (L);
                     T.End_Pos.Index := T.End_Pos.Index - 1;
                     T.End_Pos.Column := T.End_Pos.Column - 1;
                  end if;
                  L.State := Stream_End'Access;
                  exit Multiline_Loop;
               when others => null;
            end case;
         end loop Inline_Loop;
         End_Line (L);
         declare
            Newlines : Positive := 1;
         begin
            Newline_Loop : loop
               case Start_Line (L) is
                  when Content =>
                     if L.Pos - L.Line_Start - 1 <= L.Indentation then
                        L.State := After_Newline_State;
                        exit Multiline_Loop;
                     end if;
                     exit Newline_Loop;
                  when Directives_End_Marker =>
                     L.State := Line_Dir_End'Access;
                     exit Multiline_Loop;
                  when Document_End_Marker =>
                     L.State := Line_Doc_End'Access;
                     exit Multiline_Loop;
                  when Stream_End =>
                     exit Multiline_Loop;
                  when Comment =>
                     End_Line (L);
                     L.State := Line_Start'Access;
                     exit Multiline_Loop;
                  when Newline =>
                     End_Line (L);
               end case;
               Newlines := Newlines + 1;
            end loop Newline_Loop;
            if
              (L.Cur = ':' and then not Next_Is_Plain_Safe (L)) or else
              L.Cur = '#' or else (L.Cur in Flow_Indicator and L.Flow_Depth > 0)
            then
               L.State := After_Newline_State;
               exit Multiline_Loop;
            end if;
            L.Seen_Multiline := True;
            if Newlines = 1 then
               Add (Result, ' ');
            else
               Add (Result, (1 .. Newlines - 1 => Line_Feed));
            end if;
         end;
      end loop Multiline_Loop;
      L.Value := New_Content_From (L.Pool, Result);
   end Read_Plain_Scalar;

   procedure Process_Quoted_Whitespace (L : in out Lexer; Init : Natural;
                                        Result : in out Out_Buffer_Type) is
      Newlines : Natural := Init;
      Before_Space : constant Positive := Result.Pos;
   begin
      loop
         case L.Cur is
            when ' ' =>
               Add (Result, ' ');
            when Line_Feed =>
               Handle_LF (L);
               exit;
            when Carriage_Return =>
               Handle_CR (L);
               exit;
            when others =>
               return;
         end case;
         L.Cur := Next (L);
      end loop;
      L.Seen_Multiline := True;
      Result.Pos := Before_Space;
      loop
         case L.Cur is
            when ' ' => L.Cur := Next (L);
            when Line_Feed =>
               Handle_LF (L);
               Newlines := Newlines + 1;
            when Carriage_Return =>
               Handle_CR (L);
               Newlines := Newlines + 1;
            when others =>
               exit;
         end case;
      end loop;
      if Newlines = 0 then
         null;
      elsif Newlines = 1 then
         Add (Result, ' ');
      else
         Add (Result, (1 .. Newlines - 1 => Line_Feed));
      end if;
   end Process_Quoted_Whitespace;

   procedure Read_Single_Quoted_Scalar (L : in out Lexer; T : out Token) is
      Result : Out_Buffer_Type (L.Buffer.all'Length);
   begin
      L.Seen_Multiline := False;
      Start_Token (L);
      if L.Proposed_Indentation /= -1 then
         L.Indentation := L.Proposed_Indentation;
         L.Proposed_Indentation := -1;
      end if;
      L.Cur := Next (L);
      loop
         case L.Cur is
            when End_Of_Input =>
               raise Lexer_Error with
                 "Unexpected end of input (quoted string not closed)";
            when ''' =>
               L.Cur := Next (L);
               if L.Cur = ''' then
                  Add (Result, ''');
                  L.Cur := Next (L);
               else
                  exit;
               end if;
            when ' ' | Line_Feed | Carriage_Return =>
               Process_Quoted_Whitespace (L, 1, Result);
            when others =>
               Add (Result, L.Cur);
               L.Cur := Next (L);
         end case;
      end loop;
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Single_Quoted_Scalar);
      L.Value := New_Content_From (L.Pool, Result);
   end Read_Single_Quoted_Scalar;

   procedure Read_Hex_Sequence (L : in out Lexer; Length : Positive;
                                Result : in out Out_Buffer_Type) is
      Char_Pos : Natural := 0;
   begin
      for Exponent in reverse 0 .. Length - 1 loop
         L.Cur := Next (L);
         case L.Cur is
            when Digit =>
               Char_Pos := Char_Pos + (16 ** Exponent) *
                 (Character'Pos (L.Cur) - Character'Pos ('0'));
            when 'a' .. 'f' =>
               Char_Pos := Char_Pos + (16 ** Exponent) *
                 (Character'Pos (L.Cur) - Character'Pos ('a') + 10);
            when 'A' .. 'F' =>
               Char_Pos := Char_Pos + (16 ** Exponent) *
                 (Character'Pos (L.Cur) - Character'Pos ('A') + 10);
            when others =>
               raise Lexer_Error with
                 "Invalid character in hex escape sequence: " &
                 Escaped (L.Cur);
         end case;
      end loop;
      Add (Result, Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (
           "" & Wide_Wide_Character'Val (Char_Pos)));
   end Read_Hex_Sequence;

   procedure Read_Double_Quoted_Scalar (L : in out Lexer; T : out Token) is
      Result : Out_Buffer_Type (L.Buffer.all'Length);
   begin
      L.Seen_Multiline := False;
      Start_Token (L);
      if L.Proposed_Indentation /= -1 then
         L.Indentation := L.Proposed_Indentation;
         L.Proposed_Indentation := -1;
      end if;
      L.Cur := Next (L);
      loop
         <<Handle_Char>>
         case L.Cur is
            when End_Of_Input =>
               raise Lexer_Error with
                 "Unexpected end of input (quoted string not closed)";
            when '\' =>
               L.Cur := Next (L);
               case L.Cur is
                  when '0' => Add (Result, Character'Val (0));
                  when 'a' => Add (Result, Character'Val (7));
                  when 'b' => Add (Result, Character'Val (8));
                  when 't' | Character'Val (9) =>
                     Add (Result, Character'Val (9));
                  when 'n' => Add (Result, Line_Feed);
                  when 'v' => Add (Result, Character'Val (11));
                  when 'f' => Add (Result, Character'Val (12));
                  when 'r' => Add (Result, Carriage_Return);
                  when 'e' => Add (Result, Character'Val (27));
                  when ' ' | '"' | '/' | '\' => Add (Result, L.Cur);
                  when 'N' => Add (Result, Next_Line);
                  when '_' => Add (Result, Non_Breaking_Space);
                  when 'L' => Add (Result, Line_Separator);
                  when 'P' => Add (Result, Paragraph_Separator);
                  when 'x' => Read_Hex_Sequence (L, 2, Result);
                  when 'u' => Read_Hex_Sequence (L, 4, Result);
                  when 'U' => Read_Hex_Sequence (L, 8, Result);
                  when Line_Feed | Carriage_Return =>
                     Process_Quoted_Whitespace (L, 0, Result);
                     goto Handle_Char;
                  when others =>
                     raise Lexer_Error with
                       "Illegal character in escape sequence: " &
                       Escaped (L.Cur);
               end case;
            when '"' => exit;
            when ' ' | Line_Feed | Carriage_Return =>
               Process_Quoted_Whitespace (L, 1, Result);
               goto Handle_Char;
            when others =>
               Add (Result, L.Cur);
         end case;
         L.Cur := Next (L);
      end loop;
      L.Cur := Next (L);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Double_Quoted_Scalar);
      L.Value := New_Content_From (L.Pool, Result);
   end Read_Double_Quoted_Scalar;

   procedure Read_Block_Scalar (L : in out Lexer; T : out Token) is
      type Chomp_Style is (Clip, Strip, Keep);

      Chomp : Chomp_Style := Clip;
      Indent : Natural := 0;
      Separation_Lines : Natural := 0;

      Result : Out_Buffer_Type (L.Buffer.all'Length);
   begin
      Start_Token (L);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => <>,
            Kind => (if L.Cur = '>' then Folded_Scalar else Literal_Scalar));

      --  header
      loop
         L.Cur := Next (L);
         case L.Cur is
            when '+' =>
               if Chomp /= Clip then
                  raise Lexer_Error with "Multiple chomping indicators!";
               end if;
               Chomp := Keep;
            when '-' =>
               if Chomp /= Clip then
                  raise Lexer_Error with "Multiple chomping indicators!";
               end if;
               Chomp := Strip;
            when '1' .. '9' =>
               if Indent /= 0 then
                  raise Lexer_Error with "Multiple indentation indicators!";
               end if;
               Indent := Natural'Max (0, L.Indentation) +
                 Character'Pos (L.Cur) - Character'Pos ('0');
            when ' ' =>
               while L.Cur = ' ' loop
                  L.Cur := Next (L);
               end loop;
               if not (L.Cur in Comment_Or_Line_End) then
                  raise Lexer_Error with
                    "Illegal character after block scalar header: " &
                    Escaped (L.Cur);
               end if;
               exit;
            when Line_End => exit;
            when others =>
               raise Lexer_Error with
                 "Illegal character in block scalar header: " & Escaped (L.Cur);
         end case;
      end loop;
      End_Line (L);

      --  determining indentation and leading empty lines
      declare
         Max_Leading_Spaces : Natural := 0;
      begin
         loop
            if Indent = 0 then
               while L.Cur = ' ' loop
                  L.Cur := Next (L);
               end loop;
            else
               Max_Leading_Spaces := L.Line_Start + Indent;
               while L.Cur = ' ' and L.Pos <= Max_Leading_Spaces loop
                  L.Cur := Next (L);
               end loop;
            end if;
            case L.Cur is
               when Line_Feed | Carriage_Return =>
                  T.End_Pos := Cur_Mark (L);
                  Max_Leading_Spaces :=
                    Natural'Max (Max_Leading_Spaces, L.Pos - 1 - L.Line_Start);
                  End_Line (L);
                  Separation_Lines := Separation_Lines + 1;
               when End_Of_Input =>
                  L.State := Stream_End'Access;
                  goto End_Of_Input_Target;
               when others =>
                  if Indent = 0 then
                     Indent := L.Pos - L.Line_Start - 1;
                     if Indent <= Indentation_Type'Max (0, L.Indentation) then
                        L.State := Line_Indentation'Access;
                        goto Finalize;
                     elsif Indent < Max_Leading_Spaces then
                        raise Lexer_Error with
                          "Leading all-spaces line contains too many spaces.";
                     end if;
                  elsif L.Pos - L.Line_Start - 1 < Indent then
                     goto Finalize;
                  end if;
                  exit;
            end case;
         end loop;
         if Separation_Lines > 0 then
            Add (Result, (1 .. Separation_Lines => Line_Feed));
         end if;
      end;

      --  read block scalar content
      Block_Content : loop
         --  content of line
         while not (L.Cur in Line_End) loop
            Add (Result, L.Cur);
            L.Cur := Next (L);
         end loop;
         Separation_Lines := 0;
         if L.Cur = End_Of_Input then
            L.State := Stream_End'Access;
            goto End_Of_Input_Target;
         end if;
         T.End_Pos := Cur_Mark (L);
         End_Line (L);

         --  empty lines and indentation of next line
         loop
            declare
               Indent_Pos : constant Natural := L.Line_Start + Indent;
            begin
               while L.Cur = ' ' and L.Pos - 1 < Indent_Pos loop
                  L.Cur := Next (L);
               end loop;
               case L.Cur is
                  when Carriage_Return | Line_Feed =>
                     T.End_Pos := Cur_Mark (L);
                     Separation_Lines := Separation_Lines + 1;
                     End_Line (L);
                  when End_Of_Input =>
                     L.State := Stream_End'Access;
                     goto End_Of_Input_Target;
                  when others =>
                     if L.Pos - 1 < Indent_Pos then
                        exit Block_Content;
                     else
                        exit;
                     end if;
               end case;
            end;
         end loop;

         --  line folding
         if T.Kind = Literal_Scalar then
            Add (Result, (1 .. Separation_Lines + 1 => Line_Feed));
         elsif Separation_Lines = 0 then
            Add (Result, ' ');
         else
            Add (Result, (1 .. Separation_Lines => Line_Feed));
         end if;
      end loop Block_Content;

      if L.Pos - L.Line_Start - 1 > Indentation_Type'Max (0, L.Indentation) then
         if L.Cur = '#' then
            L.State := Expect_Line_End'Access;
         else
            raise Lexer_Error with
              "This line at " & Escaped (L.Cur) & " is less indented than necessary." & L.Cur_Line'Img;
         end if;
      elsif L.Pos = L.Line_Start + 1 then
         L.State := Line_Start'Access;
      else
         L.State := Line_Indentation'Access;
      end if;

      <<Finalize>>
      T.End_Pos := Cur_Mark (L);
      goto Finish;

      <<End_Of_Input_Target>>

      --  if we encounter the stream end directly after a newline character,
      --  we must have stored the T.End_Pos beforehand because we cannot
      --  calculate it back (we do not know how long the recent line was).
      if L.Pos /= L.Line_Start + 1 then
         T.End_Pos := Cur_Mark (L);
         --  the generated End_Pos is *after* the stream end char, which is one
         --  too far; compensate here.
         T.End_Pos.Index := T.End_Pos.Index - 1;
         T.End_Pos.Column := T.End_Pos.Column - 1;
      end if;

      <<Finish>>

      T.End_Pos := Cur_Mark (L);

      --  handling trailing empty lines
      case Chomp is
         when Strip => null;
         when Clip => Add (Result, Line_Feed);
         when Keep => Add (Result, (1 .. Separation_Lines + 1 => Line_Feed));
      end case;

      L.Value := New_Content_From (L.Pool, Result);
   end Read_Block_Scalar;

   procedure Read_URI (L : in out Lexer; Restricted : Boolean) is
      Result : Out_Buffer_Type (L.Buffer.all'Length);
      End_With_Space : constant Boolean := L.Cur /= '<';
   begin
      if End_With_Space then
         if (not Restricted) and L.Cur in '[' | ']' | ',' then
            raise Lexer_Error with "Flow indicator cannot start tag prefix";
         end if;
      else
         L.Cur := Next (L);
      end if;
      loop
         case L.Cur is
            when Space_Or_Line_End =>
               if End_With_Space then
                  exit;
               else
                  raise Lexer_Error with "Unclosed verbatim tag";
               end if;
            when '%' =>
               Read_Hex_Sequence (L, 2, Result);
            when Tag_Char =>
               Add (Result, L.Cur);
            when '[' | ']' | ',' =>
               if Restricted then
                  exit;
               else
                  Add (Result, L.Cur);
               end if;
            when '!' =>
               if Restricted then
                  raise Lexer_Error with "Illegal '!' in tag suffix!";
               else
                  Add (Result, '!');
               end if;
            when '>' =>
               if End_With_Space then
                  raise Lexer_Error with "Illegal character in URI: "">""";
               else
                  L.Cur := Next (L);
                  exit;
               end if;
            when others =>
               raise Lexer_Error with "Illegal character in URI: " &
                 Escaped (L.Cur);
         end case;
         L.Cur := Next (L);
      end loop;
      L.Value := New_Content_From (L.Pool, Result);
   end Read_URI;


end Yaml.Lexing.Evaluation;
