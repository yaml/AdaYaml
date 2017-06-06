with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Yada.Lexing.Scalars is
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

   function Next_Is_Plain_Safe (L : Lexer) return Boolean is
      (case L.Buffer (L.Pos) is
         when Space_Or_Line_End => False,
         when Flow_Indicator => L.Flow_Depth = 0,
          when others => True);

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
               else
                  exit;
               end if;
            when ' ' | Line_Feed | Carriage_Return =>
               Process_Quoted_Whitespace (L, 1, Result);
            when others =>
               Add (Result, L.Cur);
         end case;
      end loop;
      L.Cur := Next (L);
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
                  Char_Pos := Char_Pos + (2 ** Exponent) *
                    (Character'Pos (L.Cur) - Character'Pos ('0'));
               when 'a' .. 'f' =>
                  Char_Pos := Char_Pos + (2 ** Exponent) *
                    (Character'Pos (L.Cur) - Character'Pos ('a') + 10);
               when 'A' .. 'F' =>
                  Char_Pos := Char_Pos + (2 ** Exponent) *
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

end Yada.Lexing.Scalars;
