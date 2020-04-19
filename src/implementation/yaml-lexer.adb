--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Lexer.Evaluation;

package body Yaml.Lexer is
   -----------------------------------------------------------------------------
   --             Initialization and buffer handling                          --
   -----------------------------------------------------------------------------

   procedure Basic_Init (L : in out Instance; Pool  : Text.Pool.Reference) is
   begin
      L.State := Outside_Doc'Access;
      L.Flow_Depth := 0;
      L.Annotation_Depth := 0;
      L.Line_Start_State := Outside_Doc'Access;
      L.Json_Enabling_State := After_Token'Access;
      L.Pool := Pool;
      L.Proposed_Indentation := -1;
   end Basic_Init;

   procedure Init
     (L : in out Instance; Input : Source.Pointer; Pool : Text.Pool.Reference;
      Initial_Buffer_Size : Positive := Default_Initial_Buffer_Size) is
   begin
      L.Init (Input, Initial_Buffer_Size);
      Basic_Init (L, Pool);
      L.Cur := Next (L);
   end Init;

   procedure Init (L : in out Instance; Input : String;
                   Pool : Text.Pool.Reference) is
   begin
      L.Init (Input);
      Basic_Init (L, Pool);
      L.Cur := Next (L);
   end Init;

   -----------------------------------------------------------------------------
   --  interface and utilities
   -----------------------------------------------------------------------------

   function Escaped (S : String) return String is
      Ret : String (1 .. S'Length * 4 + 2) := (1 => '"', others => <>);
      Retpos : Positive := 2;

      procedure Add_Escape_Sequence (C : Character) with Inline is
      begin
         Ret (Retpos .. Retpos + 1) := "\" & C;
         Retpos := Retpos + 2;
      end Add_Escape_Sequence;
   begin
      for C of S loop
         case C is
            when Line_Feed         => Add_Escape_Sequence ('l');
            when Carriage_Return   => Add_Escape_Sequence ('c');
            when '"' | ''' | '\'   => Add_Escape_Sequence (C);
            when Character'Val (9) => Add_Escape_Sequence ('t');
            when Character'Val (0) .. Character'Val (8) | Character'Val (11) |
                 Character'Val (12) | Character'Val (14) .. Character'Val (31)
               =>
               Add_Escape_Sequence ('x');
               declare
                  type Byte is range 0 .. 255;
                  Charpos : constant Byte := Character'Pos (C);
               begin
                  Ret (Retpos .. Retpos + 1) :=
                    (Character'Val (Charpos / 16 + Character'Pos ('0'))) &
                    (Character'Val (Charpos mod 16 + Character'Pos ('0')));
                  Retpos := Retpos + 2;
               end;
            when others =>
               Ret (Retpos) := C;
               Retpos := Retpos + 1;
         end case;
      end loop;
      Ret (Retpos) := '"';
      return Ret (1 .. Retpos);
   end Escaped;

   function Escaped (C : Character) return String is
     (Escaped ("" & C));

   function Escaped (C : Text.Reference) return String is
     (Escaped (C.Value));

   function Next_Is_Plain_Safe (L : Instance) return Boolean is
      (case L.Buffer (L.Pos) is
         when Space_Or_Line_End => False,
          when Flow_Indicator => L.Flow_Depth + L.Annotation_Depth = 0,
          when ')' => L.Annotation_Depth = 0,
          when others => True);

   function Next_Token (L : in out Instance) return Token is
      Ret : Token;
   begin
      loop
         exit when L.State.all (L, Ret);
      end loop;
      return Ret;
   end Next_Token;

   function Short_Lexeme (L : Instance) return String is
      (L.Buffer (L.Token_Start .. L.Pos - 2));

   function Full_Lexeme (L : Instance) return String is
     (L.Buffer (L.Token_Start - 1 .. L.Pos - 2));

   procedure Start_Token (L : in out Instance) is
   begin
      L.Token_Start := L.Pos;
      L.Token_Start_Mark := Cur_Mark (L);
   end Start_Token;

   function Cur_Mark (L : Instance; Offset : Integer := -1) return Mark is
     ((Line =>  L.Cur_Line,
       Column => L.Pos + 1 - L.Line_Start + Offset,
       Index => L.Prev_Lines_Chars + L.Pos + 1 - L.Line_Start + Offset));

   function Current_Content (L : Instance) return Text.Reference is
     (L.Value);

   function Escaped_Current (L : Instance) return String is
      (Escaped (L.Value));

   function Current_Indentation (L : Instance) return Indentation_Type is
     (L.Pos - L.Line_Start - 1);

   function Recent_Indentation (L : Instance) return Indentation_Type is
     (L.Indentation);

   function Last_Scalar_Was_Multiline (L : Instance) return Boolean is
     (L.Seen_Multiline);

   function Recent_Start_Mark (L : Instance) return Mark is
     (L.Token_Start_Mark);

   --  to be called whenever a '-' is read as first character in a line. this
   --  function checks for whether this is a directives end marker ('---'). if
   --  yes, the lexer position is updated to be after the marker.
   function Is_Directives_End (L : in out Instance) return Boolean is
      Peek : Positive := L.Pos;
   begin
      if L.Buffer (Peek) = '-' then
         Peek := Peek + 1;
         if L.Buffer (Peek) = '-' then
            Peek := Peek + 1;
            if L.Buffer (Peek) in Space_Or_Line_End then
               L.Pos := Peek;
               L.Cur := Next (L);
               return True;
            end if;
         end if;
      end if;
      return False;
   end Is_Directives_End;

   --  similar to Hyphen_Line_Type, this function checks whether, when a line
   --  begin with a '.', that line contains a document end marker ('...'). if
   --  yes, the lexer position is updated to be after the marker.
   function Is_Document_End (L : in out Instance) return Boolean is
      Peek : Positive := L.Pos;
   begin
      if L.Buffer (Peek) = '.' then
         Peek := Peek + 1;
         if L.Buffer (Peek) = '.' then
            Peek := Peek + 1;
            if L.Buffer (Peek) in Space_Or_Line_End then
               L.Pos := Peek;
               L.Cur := Next (L);
               return True;
            end if;
         end if;
      end if;
      return False;
   end Is_Document_End;

   function Start_Line (L : in out Instance) return Line_Start_Kind is
   begin
      case L.Cur is
         when '-' =>
            return (if Is_Directives_End (L) then Directives_End_Marker else
                       Content);
         when '.' =>
            return (if Is_Document_End (L) then Document_End_Marker else
                       Content);
         when others =>
            while L.Cur = ' ' loop
               L.Cur := Next (L);
            end loop;
            return (case L.Cur is
                    when '#' => Comment,
                    when Line_Feed | Carriage_Return => Newline,
                    when End_Of_Input => Stream_End,
                    when others => Content);
      end case;
   end Start_Line;

   -----------------------------------------------------------------------------
   --                            Tokenization                                 --
   -----------------------------------------------------------------------------

   function Outside_Doc (L : in out Instance; T : out Token) return Boolean is
   begin
      case L.Cur is
         when '%' =>
            Start_Token (L);
            loop
               L.Cur := Next (L);
               exit when L.Cur in Space_Or_Line_End;
            end loop;
            T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                  Kind => <>);
            declare
               Name : constant String := Short_Lexeme (L);
            begin
               if Name = "YAML" then
                  L.State := Yaml_Version'Access;
                  T.Kind := Yaml_Directive;
                  return True;
               elsif Name = "TAG" then
                  L.State := Tag_Shorthand'Access;
                  T.Kind := Tag_Directive;
                  return True;
               else
                  L.State := Unknown_Directive'Access;
                  T.Kind := Unknown_Directive;
                  return True;
               end if;
            end;
         when '-' =>
            Start_Token (L);
            if Is_Directives_End (L) then
               L.State := After_Token'Access;
               T.Kind := Directives_End;
            else
               L.State := Indentation_Setting_Token'Access;
               T.Kind := Indentation;
            end if;
            T.Start_Pos := L.Token_Start_Mark;
            T.End_Pos := Cur_Mark (L);
            L.Indentation := -1;
            L.Line_Start_State := Line_Start'Access;
            return True;
         when '.' =>
            Start_Token (L);
            if Is_Document_End (L) then
               L.State := Expect_Line_End'Access;
               T.Kind := Document_End;
            else
               L.State := Indentation_Setting_Token'Access;
               L.Line_Start_State := Line_Start'Access;
               L.Indentation := -1;
               T.Kind := Indentation;
            end if;
            T.Start_Pos := L.Token_Start_Mark;
            T.End_Pos := Cur_Mark (L);
            return True;
         when others =>
            Start_Token (L);
            while L.Cur = ' ' loop
               L.Cur := Next (L);
            end loop;
            if L.Cur in Comment_Or_Line_End then
               L.State := Expect_Line_End'Access;
               return False;
            end if;
            T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                  Kind => Indentation);
            L.Indentation := -1;
            L.State := Indentation_Setting_Token'Access;
            L.Line_Start_State := Line_Start'Access;
            return True;
      end case;
   end Outside_Doc;

   function Yaml_Version (L : in out Instance; T : out Token) return Boolean is
      procedure Read_Numeric_Subtoken is
      begin
         if not (L.Cur in Digit) then
            raise Lexer_Error with "Illegal character in YAML version string: " &
              Escaped (L.Cur);
         end if;
         loop
            L.Cur := Next (L);
            exit when not (L.Cur in Digit);
         end loop;
      end Read_Numeric_Subtoken;
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      Start_Token (L);
      Read_Numeric_Subtoken;
      if L.Cur /= '.' then
         raise Lexer_Error with "Illegal character in YAML version string: " &
           Escaped (L.Cur);
      end if;
      L.Cur := Next (L);
      Read_Numeric_Subtoken;
      if not (L.Cur in Space_Or_Line_End) then
         raise Lexer_Error with "Illegal character in YAML version string: " &
           Escaped (L.Cur);
      end if;
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Directive_Param);
      L.State := Expect_Line_End'Access;
      return True;
   end Yaml_Version;

   function Tag_Shorthand (L : in out Instance; T : out Token) return Boolean is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      if L.Cur /= '!' then
         raise Lexer_Error with
           "Illegal character, tag shorthand must start with ""!"":" &
           Escaped (L.Cur);
      end if;
      Start_Token (L);
      L.Cur := Next (L);
      if L.Cur /= ' ' then
         while L.Cur in Tag_Shorthand_Char loop
            L.Cur := Next (L);
         end loop;
         if L.Cur /= '!' then
            if L.Cur in Space_Or_Line_End then
               raise Lexer_Error with "Tag shorthand must end with ""!"".";
            else
               raise Lexer_Error with "Illegal character in tag shorthand: " &
                 Escaped (L.Cur);
            end if;
         end if;
         L.Cur := Next (L);
         if L.Cur /= ' ' then
            raise Lexer_Error with "Missing space after tag shorthand";
         end if;
      end if;
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Tag_Handle);
      L.State := At_Tag_Uri'Access;
      return True;
   end Tag_Shorthand;

   function At_Tag_Uri (L : in out Instance; T : out Token) return Boolean is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      Start_Token (L);
      if L.Cur = '<' then
         raise Lexer_Error with "Illegal character in tag prefix: " &
           Escaped (L.Cur);
      end if;
      Evaluation.Read_URI (L, False);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Suffix);
      L.State := Expect_Line_End'Access;
      return True;
   end At_Tag_Uri;

   function Unknown_Directive (L : in out Instance; T : out Token) return Boolean
   is begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      if L.Cur in Comment_Or_Line_End then
         L.State := Expect_Line_End'Access;
         return False;
      end if;
      Start_Token (L);
      loop
         L.Cur := Next (L);
         exit when L.Cur in Space_Or_Line_End;
      end loop;
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Directive_Param);
      return True;
   end Unknown_Directive;

   procedure End_Line (L : in out Instance) is
   begin
      loop
         case L.Cur is
            when Line_Feed =>
               Handle_LF (L);
               L.Cur := L.Next;
               L.State := L.Line_Start_State;
               exit;
            when Carriage_Return =>
               Handle_CR (L);
               L.Cur := L.Next;
               L.State := L.Line_Start_State;
               exit;
            when End_Of_Input =>
               L.State := Stream_End'Access;
               exit;
            when '#' =>
               loop
                  L.Cur := Next (L);
                  exit when L.Cur in Line_End;
               end loop;
            when others => null; --  forbidden by precondition
         end case;
      end loop;
   end End_Line;

   function Expect_Line_End (L : in out Instance; T : out Token) return Boolean is
      pragma Unreferenced (T);
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      if not (L.Cur in Comment_Or_Line_End) then
         raise Lexer_Error with
           "Unexpected character (expected line end): " & Escaped (L.Cur);
      end if;
      End_Line (L);
      return False;
   end Expect_Line_End;

   function Stream_End (L : in out Instance; T : out Token) return Boolean is
   begin
      Start_Token (L);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Stream_End);
      return True;
   end Stream_End;

   function Line_Start (L : in out Instance; T : out Token) return Boolean is
   begin
      case Start_Line (L) is
         when Directives_End_Marker =>
            return Line_Dir_End (L, T);
         when Document_End_Marker =>
            return Line_Doc_End (L, T);
         when Comment | Newline =>
            End_Line (L);
            return False;
         when Stream_End =>
            L.State := Stream_End'Access;
            return False;
         when Content =>
            return Line_Indentation (L, T);
      end case;
   end Line_Start;

   function Flow_Line_Start (L : in out Instance; T : out Token) return Boolean is
      pragma Unreferenced (T);
      Indent : Natural;
   begin
      case L.Cur is
         when '-' =>
            if Is_Directives_End (L) then
               raise Lexer_Error with
                 "Directives end marker before end of flow content";
            else
               Indent := 0;
            end if;
         when '.' =>
            if Is_Document_End (L) then
               raise Lexer_Error with
                 "Document end marker before end of flow content";
            else
               Indent := 0;
            end if;
         when others =>
            while L.Cur = ' ' loop
               L.Cur := Next (L);
            end loop;
            Indent := L.Pos - L.Line_Start - 1;
      end case;
      if Indent <= L.Indentation then
         raise Lexer_Error with
           "Too few indentation spaces (must surpass surrounding block element)" & L.Indentation'Img;
      end if;
      L.State := Inside_Line'Access;
      return False;
   end Flow_Line_Start;

   function Flow_Line_Indentation (L : in out Instance; T : out Token)
                                   return Boolean is
      pragma Unreferenced (T);
   begin
      if L.Pos - L.Line_Start - 1 < L.Indentation then
         raise Lexer_Error with
           "Too few indentation spaces (must surpass surrounding block element)";
      end if;
      L.State := Inside_Line'Access;
      return False;
   end Flow_Line_Indentation;

   procedure Check_Indicator_Char (L : in out Instance; Kind : Token_Kind;
                                   T : out Token) is
   begin
      if Next_Is_Plain_Safe (L) then
         Evaluation.Read_Plain_Scalar (L, T);
      else
         Start_Token (L);
         L.Cur := Next (L);
         T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
               Kind => Kind);
         L.State := Before_Indentation_Setting_Token'Access;
      end if;
   end Check_Indicator_Char;

   procedure Enter_Flow_Collection (L : in out Instance; T : out Token;
                                    Kind : Token_Kind) is
   begin
      Start_Token (L);
      if L.Flow_Depth + L.Annotation_Depth = 0 then
         L.Json_Enabling_State := After_Json_Enabling_Token'Access;
         L.Line_Start_State := Flow_Line_Start'Access;
         L.Proposed_Indentation := -1;
      end if;
      L.Flow_Depth := L.Flow_Depth + 1;
      L.State := After_Token'Access;
      L.Cur := Next (L);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Kind);
   end Enter_Flow_Collection;

   procedure Leave_Flow_Collection (L : in out Instance; T : out Token;
                                    Kind : Token_Kind) is
   begin
      Start_Token (L);
      if L.Flow_Depth = 0 then
         raise Lexer_Error with "No flow collection to leave!";
      end if;
      L.Flow_Depth := L.Flow_Depth - 1;
      if L.Flow_Depth + L.Annotation_Depth = 0 then
         L.Json_Enabling_State := After_Token'Access;
         L.Line_Start_State := Line_Start'Access;
      end if;
      L.State := L.Json_Enabling_State;
      L.Cur := Next (L);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Kind);
   end Leave_Flow_Collection;

   procedure Read_Namespace (L : in out Instance; T : out Token;
                             NS_Char : Character; Kind : Token_Kind) with
     Pre => L.Cur = NS_Char is
   begin
      Start_Token (L);
      L.Cur := Next (L);
      if L.Cur = '<' then
         raise Lexer_Error with "Verbatim URIs not supported in YAML 1.3";
      else
         --  we need to scan for a possible second NS_Char in case this is not a
         --  primary tag handle. We must lookahead here because there may be
         --  URI characters in the suffix that are not allowed in the handle.
         declare
            Handle_End : Positive := L.Token_Start;
         begin
            loop
               if L.Buffer (Handle_End) in Space_Or_Line_End | Flow_Indicator |
                 Annotation_Param_Indicator
               then
                  Handle_End := L.Token_Start;
                  L.Pos := L.Pos - 1;
                  exit;
               elsif L.Buffer (Handle_End) = NS_Char then
                  Handle_End := Handle_End + 1;
                  exit;
               else
                  Handle_End := Handle_End + 1;
               end if;
            end loop;
            while L.Pos < Handle_End loop
               L.Cur := Next (L);
               if not (L.Cur in Tag_Shorthand_Char | NS_Char) then
                  raise Lexer_Error with "Illegal character in tag handle: " &
                    Escaped (L.Cur);
               end if;
            end loop;
            L.Cur := Next (L);
            T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                  Kind => Kind);
            L.State := At_Suffix'Access;
         end;
      end if;
   end Read_Namespace;

   procedure Read_Anchor_Name (L : in out Instance) is
   begin
      Start_Token (L);
      loop
         L.Cur := Next (L);
         exit when not (L.Cur in Ascii_Char | Digit | '-' | '_');
      end loop;
      if not (L.Cur in Space_Or_Line_End | Flow_Indicator | ')') then
         raise Lexer_Error with "Illegal character in anchor: " &
           Escaped (L.Cur);
      elsif L.Pos = L.Token_Start + 1 then
         raise Lexer_Error with "Anchor name must not be empty";
      end if;
      L.State := After_Token'Access;
   end Read_Anchor_Name;

   function Inside_Line (L : in out Instance; T : out Token) return Boolean is
   begin
      case L.Cur is
         when ':' =>
            Check_Indicator_Char (L, Map_Value_Ind, T);
            if T.Kind = Map_Value_Ind and then L.Proposed_Indentation /= -1 then
               --  necessary in the case of an empty scalar with node props
               --  in an implicit block map key
               L.Indentation := L.Proposed_Indentation;
               L.Proposed_Indentation := -1;
            end if;
            return True;
         when '?' =>
            Check_Indicator_Char (L, Map_Key_Ind, T);
            return True;
         when '-' =>
            Check_Indicator_Char (L, Seq_Item_Ind, T);
            return True;
         when Comment_Or_Line_End =>
            End_Line (L);
            return False;
         when '"' =>
            Evaluation.Read_Double_Quoted_Scalar (L, T);
            L.State := L.Json_Enabling_State;
            return True;
         when ''' =>
            Evaluation.Read_Single_Quoted_Scalar (L, T);
            L.State := L.Json_Enabling_State;
            return True;
         when '>' | '|' =>
            if L.Flow_Depth + L.Annotation_Depth > 0 then
               Evaluation.Read_Plain_Scalar (L, T);
            else
               Evaluation.Read_Block_Scalar (L, T);
            end if;
            return True;
         when '{' =>
            Enter_Flow_Collection (L, T, Flow_Map_Start);
            return True;
         when '}' =>
            Leave_Flow_Collection (L, T, Flow_Map_End);
            return True;
         when '[' =>
            Enter_Flow_Collection (L, T, Flow_Seq_Start);
            return True;
         when ']' =>
            Leave_Flow_Collection (L, T, Flow_Seq_End);
            return True;
         when ')' =>
            Start_Token (L);
            if L.Annotation_Depth > 0 then
               L.Annotation_Depth := L.Annotation_Depth - 1;
               if L.Flow_Depth + L.Annotation_Depth = 0 then
                  L.Json_Enabling_State := After_Token'Access;
                  L.Line_Start_State := Line_Start'Access;
               end if;
               L.State := After_Token'Access;
               L.Cur := Next (L);
               T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                     Kind => Params_End);
            else
               Evaluation.Read_Plain_Scalar (L, T);
            end if;
            return True;
         when ',' =>
            Start_Token (L);
            L.Cur := Next (L);
            T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                  Kind => Flow_Separator);
            L.State := After_Token'Access;
            return True;
         when '!' =>
            Read_Namespace (L, T, '!', Tag_Handle);
            return True;
         when '&' =>
            Read_Anchor_Name (L);
            T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                  Kind => Anchor);
            return True;
         when '*' =>
            Read_Anchor_Name (L);
            T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                  Kind => Alias);
            return True;
         when '@' =>
            Read_Namespace (L, T, '@', Annotation_Handle);
            return True;
         when '`' =>
            raise Lexer_Error with
              "Reserved characters cannot start a plain scalar.";
         when others =>
            Evaluation.Read_Plain_Scalar (L, T);
            return True;
      end case;
   end Inside_Line;

   function Indentation_Setting_Token (L : in out Instance; T : out Token)
                                       return Boolean is
      Cached_Indentation : constant Natural := L.Pos - L.Line_Start - 1;
   begin
      return Ret : constant Boolean := Inside_Line (L, T) do
         if Ret and then L.Flow_Depth + L.Annotation_Depth = 0 then
            if T.Kind in Node_Property_Kind then
               L.Proposed_Indentation := Cached_Indentation;
            else
               L.Indentation := Cached_Indentation;
            end if;
         end if;
      end return;
   end Indentation_Setting_Token;

   function After_Token (L : in out Instance; T : out Token) return Boolean is
      pragma Unreferenced (T);
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      if L.Cur in Comment_Or_Line_End then
         End_Line (L);
      else
         L.State := Inside_Line'Access;
      end if;
      return False;
   end After_Token;

   function Before_Indentation_Setting_Token (L : in out Instance; T : out Token)
                                              return Boolean is
   begin
      if After_Token (L, T) then
         null;
      end if;
      if L.State = Inside_Line'Access then
         L.State := Indentation_Setting_Token'Access;
      end if;
      return False;
   end Before_Indentation_Setting_Token;

   function After_Json_Enabling_Token (L : in out Instance; T : out Token)
                                       return Boolean is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      loop
         case L.Cur is
            when ':' =>
               Start_Token (L);
               L.Cur := Next (L);
               T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
                     Kind => Map_Value_Ind);
               L.State := After_Token'Access;
               return True;
            when '#' | Carriage_Return | Line_Feed =>
               End_Line (L);
               if Flow_Line_Start (L, T) then null; end if;
            when End_Of_Input =>
               L.State := Stream_End'Access;
               return False;
            when others =>
               L.State := Inside_Line'Access;
               return False;
         end case;
      end loop;
   end After_Json_Enabling_Token;

   function Line_Indentation (L : in out Instance; T : out Token)
                              return Boolean is
   begin
      T := (Start_Pos => (Line => L.Cur_Line,
                          Column => 1, Index => L.Prev_Lines_Chars),
            End_Pos => Cur_Mark (L), Kind => Indentation);
      L.State := Indentation_Setting_Token'Access;
      return True;
   end Line_Indentation;

   function Line_Dir_End (L : in out Instance; T : out Token)
                          return Boolean is
   begin
      T := (Start_Pos => (Line => L.Cur_Line,
                          Column => 1, Index => L.Prev_Lines_Chars),
            End_Pos => Cur_Mark (L), Kind => Directives_End);
      L.State := After_Token'Access;
      L.Indentation := -1;
      L.Proposed_Indentation := -1;
      return True;
   end Line_Dir_End;

   --  similar to Indentation_After_Plain_Scalar, but used for a document end
   --  marker ending a plain scalar.
   function Line_Doc_End (L : in out Instance; T : out Token)
                          return Boolean is
   begin
      T := (Start_Pos => (Line => L.Cur_Line,
                          Column => 1, Index => L.Prev_Lines_Chars),
            End_Pos => Cur_Mark (L), Kind => Document_End);
      L.State := Expect_Line_End'Access;
      L.Line_Start_State := Outside_Doc'Access;
      return True;
   end Line_Doc_End;

   function At_Suffix (L : in out Instance; T : out Token) return Boolean is
   begin
      Start_Token (L);
      while L.Cur in Suffix_Char loop
         L.Cur := Next (L);
      end loop;
      L.Value := L.Pool.From_String (L.Full_Lexeme);
      T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
            Kind => Suffix);
      L.State := After_Suffix'Access;
      return True;
   end At_Suffix;

   function After_Suffix (L : in out Instance; T : out Token) return Boolean is
   begin
      L.State := After_Token'Access;
      if L.Cur = '(' then
         Start_Token (L);
         T := (Start_Pos => L.Token_Start_Mark, End_Pos => Cur_Mark (L),
               Kind => Params_Start);
         L.Annotation_Depth := L.Annotation_Depth + 1;
         L.Proposed_Indentation := -1;
         L.Cur := Next (L);
         return True;
      else
         return False;
      end if;
   end After_Suffix;

end Yaml.Lexer;
