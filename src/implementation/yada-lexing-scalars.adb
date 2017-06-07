with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Yada.Lexing.Scalars is
   -----------------------------------------------------------------------------
   --  constant UTF-8 strings that may be generated from escape sequences
   -----------------------------------------------------------------------------

   Next_Line : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
     Ada.Strings.UTF_Encoding.Strings.Encode ("" & Character'Val (16#85#));
   Non_Breaking_Space : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
     Ada.Strings.UTF_Encoding.Strings.Encode ("" & Character'Val (16#A0#));
   Line_Separator : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
     Ada.Strings.UTF_Encoding.Wide_Strings.Encode
       ("" & Wide_Character'Val (16#2028#));
   Paragraph_Separator : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
     Ada.Strings.UTF_Encoding.Wide_Strings.Encode
       ("" & Wide_Character'Val (16#2029#));

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

   function New_String_From (O : Out_Buffer_Type) return access UTF_8_String
     with Inline is
   begin
      return S : constant access UTF_8_String :=
        new UTF_8_String (1 .. O.Pos - 1) do
         S.all := O.Content (1 .. O.Pos - 1);
      end return;
   end New_String_From;

   -----------------------------------------------------------------------------
   --  implementation
   -----------------------------------------------------------------------------

   procedure Read_Plain_Scalar (L : in out Lexer) is
      --  our scalar cannot possibly have more content than the size of our
      --  buffer. Therefore, we read its value into a string of the same size
      --  so we never have to do any bounds checking and growing of the string.
      Result : Out_Buffer_Type (L.Buffer.all'Length);
   begin
      L.Token_Start := L.Pos;
      Multiline_Loop : loop
         Inline_Loop : loop
            Add (Result, L.Cur);
            L.Cur := Next (L);
            case L.Cur is
               when ' ' =>
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
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when Flow_Indicator =>
                  if L.Flow_Depth > 0 then
                     L.State := Inside_Line'Access;
                     exit Multiline_Loop;
                  end if;
               when Line_Feed | Carriage_Return =>
                  exit Inline_Loop;
               when End_Of_Input =>
                  L.State := Stream_End'Access;
                  exit Multiline_Loop;
               when others => null;
            end case;
         end loop Inline_Loop;
         End_Line (L);
         declare
            Newlines : Positive := 1;
            T : Token;
         begin
            Newline_Loop : loop
               if Line_Start (L, T) then
                  case T is
                     when Indentation =>
                        if L.Pos - L.Line_Start - 1 <= L.Indentation then
                           L.State := Line_Indentation'Access;
                           exit Multiline_Loop;
                        end if;
                        exit Newline_Loop;
                     when Directives_End =>
                        L.State := Line_Dir_End'Access;
                        exit Multiline_Loop;
                     when Document_End =>
                        L.State := Line_Doc_End'Access;
                        exit Multiline_Loop;
                     when others =>
                        null; --  never happens
                  end case;
               end if;
               Newlines := Newlines + 1;
            end loop Newline_Loop;
            if
              (L.Cur = ':' and then not Next_Is_Plain_Safe (L)) or else
              L.Cur = '#'
            then
               L.State := Line_Indentation'Access;
               exit Multiline_Loop;
            end if;
            if Newlines = 1 then
               Add (Result, ' ');
            else
               Add (Result, (1 .. Newlines - 1 => Line_Feed));
            end if;
         end;
      end loop Multiline_Loop;
      L.Scalar_Content := New_String_From (Result);
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

   procedure Read_Single_Quoted_Scalar (L : in out Lexer) is
      Result : Out_Buffer_Type (L.Buffer.all'Length);
   begin
      L.Token_Start := L.Pos;
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
      L.Scalar_Content := New_String_From (Result);
   end Read_Single_Quoted_Scalar;

   procedure Read_Double_Quoted_Scalar (L : in out Lexer) is
      Result : Out_Buffer_Type (L.Buffer.all'Length);

      procedure Read_Unicode_Sequence (Length : Positive) is
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
                    "Invalid character in unicode escape sequence: " &
                    Escaped (L.Cur);
            end case;
         end loop;
         Add (Result, Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (
              "" & Wide_Wide_Character'Val (Char_Pos)));
      end Read_Unicode_Sequence;
   begin
      L.Token_Start := L.Pos;
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
                  when 'x' => Read_Unicode_Sequence (2);
                  when 'u' => Read_Unicode_Sequence (4);
                  when 'U' => Read_Unicode_Sequence (8);
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
      L.Scalar_Content := New_String_From (Result);
   end Read_Double_Quoted_Scalar;

   procedure Read_Block_Scalar (L : in out Lexer) is
      type Chomp_Style is (Clip, Strip, Keep);
      type Line_Style is (Literal, Folded);

      Chomp : Chomp_Style := Clip;
      Lines : constant Line_Style  := (if L.Cur = '>' then Folded else Literal);
      Indent : Natural := 0;
      Separation_Lines : Natural := 0;

      Result : Out_Buffer_Type (L.Buffer.all'Length);
   begin
      L.Token_Start := L.Pos;

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
            when Space_Or_Line_End => exit;
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
                  Max_Leading_Spaces := L.Pos - L.Line_Start;
                  End_Line (L);
                  Separation_Lines := Separation_Lines + 1;
               when End_Of_Input =>
                  L.State := Stream_End'Access;
                  goto Finish;
               when others =>
                  if Indent = 0 then
                     Indent := L.Pos - L.Line_Start - 1;
                     if Indent < L.Indentation then
                        L.State := Line_Indentation'Access;
                        goto Finish;
                     elsif Indent < Max_Leading_Spaces then
                        raise Lexer_Error with
                          "Leading all-spaces line contains too many spaces.";
                     end if;
                  elsif L.Pos - L.Line_Start - 1 < Indent then
                     goto Finish;
                  end if;
                  exit;
            end case;
            End_Line (L);
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
            goto Finish;
         end if;
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
                     Separation_Lines := Separation_Lines + 1;
                     End_Line (L);
                  when End_Of_Input =>
                     L.State := Stream_End'Access;
                     goto Finish;
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
         if Lines = Literal then
            Add (Result, (1 .. Separation_Lines + 1 => Line_Feed));
         elsif Separation_Lines = 0 then
            Add (Result, ' ');
         else
            Add (Result, (1 .. Separation_Lines => Line_Feed));
         end if;
      end loop Block_Content;

      if L.Pos - L.Line_Start - 1 > L.Indentation then
         if L.Cur = '#' then
            L.State := Expect_Line_End'Access;
         else
            raise Lexer_Error with
              "This line at " & Escaped (L.Cur) & " is less indented than necessary." & L.Cur_Line'Img;
         end if;
      else
         L.State := Line_Indentation'Access;
      end if;

      <<Finish>>

      --  handling trailing empty lines
      case Chomp is
         when Strip => null;
         when Clip => Add (Result, Line_Feed);
         when Keep => Add (Result, (1 .. Separation_Lines + 1 => Line_Feed));
      end case;

      L.Scalar_Content := New_String_From (Result);
   end Read_Block_Scalar;

end Yada.Lexing.Scalars;
