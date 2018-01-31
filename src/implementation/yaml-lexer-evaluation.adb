--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Text.Builder;

package body Yaml.Lexer.Evaluation is
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
   --  implementation
   -----------------------------------------------------------------------------

   procedure Read_Plain_Scalar (L : in out Instance; T : out Token) is
      Target : Text.Builder.Reference := Text.Builder.Create (L.Pool);
      After_Newline_State : constant State_Type :=
        (if L.Flow_Depth + L.Annotation_Depth = 0 then Line_Indentation'Access
           else Flow_Line_Indentation'Access);
      Line_Start_Pos : Positive;
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
         Line_Start_Pos := L.Pos - 1;
         Inline_Loop : loop
            L.Cur := Next (L);
            case L.Cur is
               when ' ' =>
                  T.End_Pos := Cur_Mark (L);
                  declare
                     Space_Start : constant Positive := L.Pos - 1;
                  begin
                     Space_Loop : loop
                        L.Cur := Next (L);
                        case L.Cur is
                           when Line_Feed | Carriage_Return =>
                              Target.Append
                                (L.Buffer (Line_Start_Pos .. Space_Start - 1));
                              exit Inline_Loop;
                           when End_Of_Input =>
                              Target.Append
                                (L.Buffer (Line_Start_Pos .. Space_Start - 1));
                              L.State := Stream_End'Access;
                              exit Multiline_Loop;
                           when '#' =>
                              Target.Append
                                (L.Buffer (Line_Start_Pos .. Space_Start - 1));
                              L.State := Expect_Line_End'Access;
                              exit Multiline_Loop;
                           when ':' =>
                              if not Next_Is_Plain_Safe (L) then
                                 Target.Append
                                   (L.Buffer (Line_Start_Pos .. Space_Start - 1));
                                 L.State := Inside_Line'Access;
                                 exit Multiline_Loop;
                              end if;
                              exit Space_Loop;
                           when Flow_Indicator =>
                              if L.Flow_Depth + L.Annotation_Depth > 0 then
                                 Target.Append
                                   (L.Buffer (Line_Start_Pos .. Space_Start - 1));
                                 L.State := Inside_Line'Access;
                                 exit Multiline_Loop;
                              end if;
                              exit Space_Loop;
                           when ')' =>
                              if L.Annotation_Depth > 0 then
                                 Target.Append
                                   (L.Buffer (Line_Start_Pos .. Space_Start - 1));
                                 L.State := Inside_Line'Access;
                                 exit Multiline_Loop;
                              end if;
                              exit Space_Loop;
                           when ' ' => null;
                           when others => exit Space_Loop;
                        end case;
                     end loop Space_Loop;
                  end;
               when ':' =>
                  if not Next_Is_Plain_Safe (L) then
                     Target.Append
                       (L.Buffer (Line_Start_Pos .. L.Pos - 2));
                     T.End_Pos := Cur_Mark (L);
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when Flow_Indicator =>
                  if L.Flow_Depth + L.Annotation_Depth > 0 then
                     Target.Append
                       (L.Buffer (Line_Start_Pos .. L.Pos - 2));
                     T.End_Pos := Cur_Mark (L);
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when ')' =>
                  if L.Annotation_Depth > 0 then
                     Target.Append
                       (L.Buffer (Line_Start_Pos .. L.Pos - 2));
                     T.End_Pos := Cur_Mark (L);
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when Line_Feed | Carriage_Return =>
                  Target.Append
                    (L.Buffer (Line_Start_Pos .. L.Pos - 2));
                  T.End_Pos := Cur_Mark (L);
                  exit Inline_Loop;
               when End_Of_Input =>
                  Target.Append
                    (L.Buffer (Line_Start_Pos .. L.Pos - 2));
                  if L.Pos /= L.Line_Start then
                     T.End_Pos := Cur_Mark (L);
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
              L.Cur = '#' or else (L.Cur in Flow_Indicator and
                                    L.Flow_Depth + L.Annotation_Depth > 0)
              or else (L.Cur = ')' and L.Annotation_Depth > 0)
            then
               L.State := After_Newline_State;
               exit Multiline_Loop;
            end if;
            L.Seen_Multiline := True;
            if Newlines = 1 then
               Target.Append (' ');
            else
               Target.Append ((1 .. Newlines - 1 => Line_Feed));
            end if;
         end;
      end loop Multiline_Loop;
      L.Value := Target.Lock;
   end Read_Plain_Scalar;

   procedure Process_Quoted_Whitespace (L : in out Instance; Init : Natural;
                                        Target : in out Text.Builder.Reference) is
      Newlines : Natural := Init;
      First_Space : constant Positive := L.Pos - 1;
   begin
      loop
         case L.Cur is
            when ' ' => null;
            when Line_Feed =>
               Handle_LF (L);
               L.Cur := L.Next;
               exit;
            when Carriage_Return =>
               Handle_CR (L);
               L.Cur := L.Next;
               exit;
            when others =>
               Target.Append (L.Buffer (First_Space .. L.Pos - 2));
               return;
         end case;
         L.Cur := Next (L);
      end loop;
      L.Seen_Multiline := True;
      loop
         case Start_Line (L) is
            when Content | Comment => exit;
            when Directives_End_Marker =>
               raise Lexer_Error with "Illegal '---' within quoted scalar";
            when Document_End_Marker =>
               raise Lexer_Error with "Illegal '...' within quoted scalar";
            when Newline => End_Line (L);
            when Stream_End =>
               raise Lexer_Error with
                 "Unexpected end of input (quoted string not closed)";
         end case;
         Newlines := Newlines + 1;
      end loop;
      if Newlines = 0 then
         null;
      elsif Newlines = 1 then
         Target.Append (' ');
      else
         Target.Append ((1 .. Newlines - 1 => Line_Feed));
      end if;
   end Process_Quoted_Whitespace;

   procedure Read_Single_Quoted_Scalar (L : in out Instance; T : out Token) is
      Target : Text.Builder.Reference := Text.Builder.Create (L.Pool);
      Literal_Start : Positive;
   begin
      L.Seen_Multiline := False;
      Start_Token (L);
      if L.Proposed_Indentation /= -1 then
         L.Indentation := L.Proposed_Indentation;
         L.Proposed_Indentation := -1;
      end if;
      Literal_Start := L.Pos;
      L.Cur := Next (L);
      loop
         case L.Cur is
            when End_Of_Input =>
               raise Lexer_Error with
                 "Unexpected end of input (quoted string not closed)";
            when ''' =>
               Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
               L.Cur := Next (L);
               if L.Cur = ''' then
                  Target.Append (''');
                  Literal_Start := L.Pos;
                  L.Cur := Next (L);
               else
                  exit;
               end if;
            when ' ' | Line_Feed | Carriage_Return =>
               Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
               Process_Quoted_Whitespace (L, 1, Target);
               Literal_Start := L.Pos - 1;
            when others =>
               L.Cur := Next (L);
         end case;
      end loop;
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Single_Quoted_Scalar);
      L.Value := Target.Lock;
   end Read_Single_Quoted_Scalar;

   subtype Hex_Code_Point is Natural range 0 .. 16#1FFFFF#;

   procedure Read_Hex_Sequence (L : in out Instance; Length : Positive;
                                Target : in out Text.Builder.Reference) is
      Char_Pos : Hex_Code_Point := 0;
      Start_Pos : constant Positive := L.Pos;
   begin
      --  first, we make sure that this is a valid escape sequence. it is
      --  important to not calculate its value directly because that may lead
      --  to an overflow before we checked that the escape sequence is
      --  syntactically correct. We only want to report that the value is out of
      --  range if it is a valid escape sequence.
      for I in 0 .. Length - 1 loop
         if not (L.Buffer (Start_Pos + I) in Digit | 'a' .. 'f' | 'A' .. 'F')
         then
            raise Lexer_Error with
                 "Invalid character in hex escape sequence: " &
              Escaped (L.Buffer (Start_Pos + I));
         end if;
      end loop;
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
               null; --  cannot happen because of the check above
         end case;
      end loop;
      Target.Append (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (
           "" & Wide_Wide_Character'Val (Char_Pos)));
   exception
      when Constraint_Error =>
         raise Lexer_Error with
           "Invalid hex escape sequence (value too large): " &
           L.Buffer (Start_Pos .. Start_Pos + Length - 1);
   end Read_Hex_Sequence;

   procedure Read_Double_Quoted_Scalar (L : in out Instance; T : out Token) is
      Target : Text.Builder.Reference := Text.Builder.Create (L.Pool);
      Literal_Start : Positive;
   begin
      L.Seen_Multiline := False;
      Start_Token (L);
      if L.Proposed_Indentation /= -1 then
         L.Indentation := L.Proposed_Indentation;
         L.Proposed_Indentation := -1;
      end if;
      Literal_Start := L.Pos;
      L.Cur := Next (L);
      loop
         <<Handle_Char>>
         case L.Cur is
            when End_Of_Input =>
               raise Lexer_Error with
                 "Unexpected end of input (quoted string not closed)";
            when '\' =>
               Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
               L.Cur := Next (L);
               Literal_Start := L.Pos;
               case L.Cur is
                  when '0' => Target.Append (Character'Val (0));
                  when 'a' => Target.Append (Character'Val (7));
                  when 'b' => Target.Append (Character'Val (8));
                  when 't' | Character'Val (9) =>
                     Target.Append (Character'Val (9));
                  when 'n' => Target.Append (Line_Feed);
                  when 'v' => Target.Append (Character'Val (11));
                  when 'f' => Target.Append (Character'Val (12));
                  when 'r' => Target.Append (Carriage_Return);
                  when 'e' => Target.Append (Character'Val (27));
                  when ' ' | '"' | '/' | '\' => Target.Append (L.Cur);
                  when 'N' => Target.Append (Next_Line);
                  when '_' => Target.Append (Non_Breaking_Space);
                  when 'L' => Target.Append (Line_Separator);
                  when 'P' => Target.Append (Paragraph_Separator);
                  when 'x' =>
                     Read_Hex_Sequence (L, 2, Target);
                     Literal_Start := L.Pos;
                  when 'u' =>
                     Read_Hex_Sequence (L, 4, Target);
                     Literal_Start := L.Pos;
                  when 'U' =>
                     Read_Hex_Sequence (L, 8, Target);
                     Literal_Start := L.Pos;
                  when Line_Feed | Carriage_Return =>
                     Process_Quoted_Whitespace (L, 0, Target);
                     Literal_Start := L.Pos - 1;
                     goto Handle_Char;
                  when others =>
                     raise Lexer_Error with
                       "Illegal character in escape sequence: " &
                       Escaped (L.Cur);
               end case;
            when '"' =>
               Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
               exit;
            when ' ' | Line_Feed | Carriage_Return =>
               Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
               Process_Quoted_Whitespace (L, 1, Target);
               Literal_Start := L.Pos - 1;
               goto Handle_Char;
            when others => null;
         end case;
         L.Cur := Next (L);
      end loop;
      L.Cur := Next (L);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Double_Quoted_Scalar);
      L.Value := Target.Lock;
   end Read_Double_Quoted_Scalar;

   procedure Read_Block_Scalar (L : in out Instance; T : out Token) is
      type Chomp_Style is (Clip, Strip, Keep);

      Chomp : Chomp_Style := Clip;
      Indent : Natural := 0;
      Separation_Lines : Natural := 0;

      Content_Start : Positive;
      Target : Text.Builder.Reference := Text.Builder.Create (L.Pool);
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
            Target.Append ((1 .. Separation_Lines => Line_Feed));
         end if;
      end;

      --  read block scalar content
      Block_Content : loop
         --  content of line
         Content_Start := L.Pos - 1;
         while not (L.Cur in Line_End) loop
            L.Cur := Next (L);
         end loop;
         Target.Append (L.Buffer (Content_Start .. L.Pos - 2));
         Separation_Lines := 0;
         if L.Cur = End_Of_Input then
            L.State := Stream_End'Access;
            goto End_Of_Input_Target;
         end if;
         Separation_Lines := Separation_Lines + 1;
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
            Target.Append ((1 .. Separation_Lines  => Line_Feed));
         elsif Separation_Lines = 1 then
            Target.Append (' ');
         else
            Target.Append ((1 .. Separation_Lines - 1 => Line_Feed));
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
         when Clip =>
            if Target.Length > 0 then
               Target.Append (Line_Feed);
            end if;
         when Keep => Target.Append ((1 .. Separation_Lines => Line_Feed));
      end case;

      L.Value := Target.Lock;
   end Read_Block_Scalar;

   procedure Read_URI (L : in out Instance; Restricted : Boolean) is
      Target : Text.Builder.Reference := Text.Builder.Create (L.Pool);
      End_With_Space : constant Boolean := L.Cur /= '<';
      Literal_Start : Positive;
   begin
      if End_With_Space then
         if (not Restricted) and L.Cur in '[' | ']' | ',' then
            raise Lexer_Error with "Flow indicator cannot start tag prefix";
         end if;
         Literal_Start := L.Pos - 1;
      else
         Literal_Start := L.Pos;
         L.Cur := Next (L);
      end if;
      loop
         case L.Cur is
            when Space_Or_Line_End =>
               if End_With_Space then
                  Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
                  exit;
               else
                  raise Lexer_Error with "Unclosed verbatim tag";
               end if;
            when '%' =>
               Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
               Read_Hex_Sequence (L, 2, Target);
               Literal_Start := L.Pos;
            when Tag_Char => null;
            when '[' | ']' | ',' =>
               if Restricted then
                  Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
                  exit;
               end if;
            when '!' =>
               if Restricted then
                  raise Lexer_Error with "Illegal '!' in tag suffix!";
               end if;
            when '>' =>
               if End_With_Space then
                  raise Lexer_Error with "Illegal character in URI: "">""";
               else
                  Target.Append (L.Buffer (Literal_Start .. L.Pos - 2));
                  L.Cur := Next (L);
                  exit;
               end if;
            when others =>
               raise Lexer_Error with "Illegal character in URI: " &
                 Escaped (L.Cur);
         end case;
         L.Cur := Next (L);
      end loop;
      L.Value := Target.Lock;
   end Read_URI;


end Yaml.Lexer.Evaluation;
