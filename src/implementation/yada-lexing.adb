with Ada.Unchecked_Deallocation;
with Yada.Lexing.Scalars;

package body Yada.Lexing is
   -----------------------------------------------------------------------------
   --             Initialization and buffer handling                          --
   -----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation (String, Buffer_Type);

   function Next (Object : in out Lexer) return Character is
   begin
      return C : constant Character := Object.Buffer (Object.Pos) do
         Object.Pos := Object.Pos + 1;
      end return;
   end Next;

   procedure Refill_Buffer (L : in out Lexer) is
      Bytes_To_Copy : constant Natural := L.Buffer'Last + 1 - L.Sentinel;
      Fill_At : Positive := Bytes_To_Copy + 1;
      Bytes_Read : Positive;

      function Search_Sentinel return Boolean with Inline is
         Peek : Positive := L.Buffer'Last;
      begin
         while not (L.Buffer (Peek) in Line_End) loop
            if Peek = Fill_At then
               return False;
            else
               Peek := Peek - 1;
            end if;
         end loop;
         L.Sentinel := Peek + 1;
         return True;
      end Search_Sentinel;
   begin
      if Bytes_To_Copy > 0 then
         L.Buffer (1 .. Bytes_To_Copy) := L.Buffer (L.Sentinel .. L.Buffer'Last);
      end if;
      loop
         L.Input.Read_Data (L.Buffer (Fill_At .. L.Buffer'Last), Bytes_Read);
         if Bytes_Read < L.Buffer'Last - Fill_At then
            L.Sentinel := Fill_At + Bytes_Read + 1;
            L.Buffer (L.Sentinel - 1) := End_Of_Input;
            exit;
         else
            exit when Search_Sentinel;
            Fill_At := L.Buffer'Last + 1;
            declare
               New_Buffer : constant Buffer_Type :=
                 new UTF_String (1 .. 2 * L.Buffer'Last);
            begin
               New_Buffer.all (L.Buffer'Range) := L.Buffer.all;
               Free (L.Buffer);
               L.Buffer := New_Buffer;
            end;
         end if;
      end loop;
   end Refill_Buffer;

   procedure Handle_CR (L : in out Lexer) is
   begin
      if L.Buffer (L.Pos) = Line_Feed then
         L.Pos := L.Pos + 1;
      end if;
      if L.Pos = L.Sentinel then
         Refill_Buffer (L);
         L.Pos := 1;
      end if;
      L.Line_Start := L.Pos;
      L.Cur_Line := L.Cur_Line + 1;
      L.Cur := Next (L);
   end Handle_CR;

   procedure Handle_LF (L : in out Lexer) is
   begin
      if L.Pos = L.Sentinel then
         Refill_Buffer (L);
         L.Pos := 1;
      end if;
      L.Line_Start := L.Pos;
      L.Cur_Line := L.Cur_Line + 1;
      L.Cur := Next (L);
   end Handle_LF;

   function New_Lexer (Input : Sources.Source_Access; Buffer : Buffer_Type)
     return Lexer is
      (Lexer'(Input => Input, Sentinel => Buffer.all'Last + 1, Buffer => Buffer,
              Pos => Buffer.all'First, Indentation => <>, Cur_Line => 1,
              State => Outside_Doc'Access, Cur => <>, Flow_Depth => 0,
              Line_Start_State => Outside_Doc'Access, Scalar_Content => null,
              Json_Enabling_State => Inside_Line'Access,
              Token_Start => <>, Line_Start => Buffer.all'First))
     with Inline;

   function From_Source
     (Input : Sources.Source_Access;
      Initial_Buffer_Size : Positive := Default_Initial_Buffer_Size)
      return Lexer is
   begin
      return L : Lexer :=
        New_Lexer (Input, new String (1 .. Initial_Buffer_Size)) do
         Refill_Buffer (L);
         L.Cur := Next (L);
      end return;
   end From_Source;

   function From_String (Input : String) return Lexer is
   begin
      return L : Lexer :=
        New_Lexer (null, new String (1 .. Input'Length + 1)) do
         L.Buffer.all := Input & End_Of_Input;
         L.Cur := Next (L);
      end return;
   end From_String;

   function From_String (Input : not null access String) return Lexer is
   begin
      return L : Lexer := New_Lexer (null, Buffer_Type (Input)) do
         L.Cur := Next (L);
      end return;
   end From_String;

   -----------------------------------------------------------------------------
   --                            Tokenization                                 --
   -----------------------------------------------------------------------------

   function Escaped (S : String) return String is
      Ret : String (1 .. S'Length * 4) := (1 => '"', others => <>);
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

   function Escaped (C : Character) return String is (Escaped ("" & C));

   function Next_Token (L : in out Lexer) return Token is
      Ret : Token;
   begin
      loop
         exit when L.State.all (L, Ret);
      end loop;
      return Ret;
   end Next_Token;

   function Short_Lexeme (L : Lexer) return String is
      (L.Buffer (L.Token_Start .. L.Pos - 1));

   function Full_Lexeme (L : Lexer) return String is
     (L.Buffer (L.Token_Start - 1 .. L.Pos - 1));

   --  to be called whenever a '-' is read as first character in a line. this
   --  function checks for whether this is a directives end marker ('---'). if
   --  yes, the lexer position is updated to be after the marker.
   function Is_Directives_End (L : in out Lexer) return Boolean is
      Peek : Positive := L.Pos + 1;
   begin
      if L.Buffer (Peek) = '-' then
         Peek := Peek + 1;
         if L.Buffer (Peek) = '-' then
            Peek := Peek + 1;
            if L.Buffer (Peek) in Space_Or_Line_End then
               L.Pos := Peek;
               return True;
            end if;
         end if;
      end if;
      return False;
   end Is_Directives_End;

   --  similar to Hyphen_Line_Type, this function checks whether, when a line
   --  begin with a '.', that line contains a document end marker ('...'). if
   --  yes, the lexer position is updated to be after the marker.
   function Is_Document_End (L : in out Lexer) return Boolean is
      Peek : Positive := L.Pos + 1;
   begin
      if L.Buffer (Peek) = '.' then
         Peek := Peek + 1;
         if L.Buffer (Peek) = '.' then
            Peek := Peek + 1;
            if L.Buffer (Peek) in Space_Or_Line_End then
               L.Pos := Peek;
               return True;
            end if;
         end if;
      end if;
      return False;
   end Is_Document_End;

   function Outside_Doc (L : in out Lexer; T : out Token) return Boolean is
   begin
      case L.Cur is
         when '%' =>
            L.Token_Start := L.Pos;
            loop
               L.Cur := Next (L);
               exit when L.Cur in Space_Or_Line_End;
            end loop;
            declare
               Name : constant String := Short_Lexeme (L);
            begin
               if Name = "YAML" then
                  L.State := Yaml_Version'Access;
                  T := Yaml_Directive;
                  return True;
               elsif Name = "TAG" then
                  L.State := Tag_Shorthand'Access;
                  T := Tag_Directive;
                  return True;
               else
                  L.State := Unknown_Directive'Access;
                  T := Unknown_Directive;
                  return True;
               end if;
            end;
         when '-' =>
            if Is_Directives_End (L) then
               L.State := Inside_Line'Access;
               T := Directives_End;
            else
               L.State := Indentation_Setting_Token'Access;
               T := Indentation;
            end if;
            L.Indentation := -1;
            L.Line_Start_State := Line_Start'Access;
            return True;
         when '.' =>
            if Is_Document_End (L) then
               L.State := Expect_Line_End'Access;
               T := Document_End;
            else
               L.State := Indentation_Setting_Token'Access;
               L.Line_Start_State := Line_Start'Access;
               L.Indentation := -1;
               T := Indentation;
            end if;
            return True;
         when others =>
            while L.Cur = ' ' loop
               L.Cur := Next (L);
            end loop;
            if L.Cur in Comment_Or_Line_End then
               L.State := Expect_Line_End'Access;
               return False;
            end if;
            L.Indentation := -1;
            T := Indentation;
            L.State := Indentation_Setting_Token'Access;
            L.Line_Start_State := Line_Start'Access;
            return True;
      end case;
   end Outside_Doc;

   function Yaml_Version (L : in out Lexer; T : out Token) return Boolean is
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
      L.Token_Start := L.Pos;
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
      L.State := Expect_Line_End'Access;
      T := Directive_Param;
      return True;
   end Yaml_Version;

   function Tag_Shorthand (L : in out Lexer; T : out Token) return Boolean is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      if L.Cur /= '!' then
         raise Lexer_Error with
           "Illegal character, tag shorthand must start with ""!"":" &
           Escaped (L.Cur);
      end if;
      L.Token_Start := L.Pos;
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
      L.State := Tag_Uri'Access;
      T := Directive_Param;
      return True;
   end Tag_Shorthand;

   function Tag_Uri (L : in out Lexer; T : out Token) return Boolean is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      L.Token_Start := L.Pos;
      if L.Cur = '!' then
         --  local tag prefix
         L.Cur := Next (L);
      end if;
      loop
         case L.Cur is
            when Tag_Uri_Char => L.Cur := Next (L);
            when Space_Or_Line_End => exit;
            when others =>
               raise Lexer_Error with "Illegal character in tag prefix: " &
                 Escaped (L.Cur);
         end case;
      end loop;
      T := Directive_Param;
      L.Line_Start_State := Outside_Doc'Access;
      L.State := Expect_Line_End'Access;
      return True;
   end Tag_Uri;

   function Unknown_Directive (L : in out Lexer; T : out Token) return Boolean
   is begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      if L.Cur in Comment_Or_Line_End then
         L.State := Expect_Line_End'Access;
         return False;
      end if;
      L.Token_Start := L.Pos;
      loop
         L.Cur := Next (L);
         exit when L.Cur in Space_Or_Line_End;
      end loop;
      T := Directive_Param;
      return True;
   end Unknown_Directive;

   procedure End_Line (L : in out Lexer) is
   begin
      loop
         case L.Cur is
            when Line_Feed =>
               Handle_LF (L);
               L.State := L.Line_Start_State;
               exit;
            when Carriage_Return =>
               Handle_CR (L);
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

   function Expect_Line_End (L : in out Lexer; T : out Token) return Boolean is
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

   function Stream_End (L : in out Lexer; T : out Token) return Boolean is
   begin
      L.Token_Start := L.Pos;
      T := Stream_End;
      return True;
   end Stream_End;

   function Line_Start (L : in out Lexer; T : out Token) return Boolean is
   begin
      case L.Cur is
         when '-' =>
            if Is_Directives_End (L) then
               return Line_Dir_End (L, T);
            else
               return Line_Indentation (L, T);
            end if;
         when '.' =>
            if Is_Document_End (L) then
               return Line_Doc_End (L, T);
            else
               return Line_Indentation (L, T);
            end if;
         when others =>
            while L.Cur = ' ' loop
               L.Cur := Next (L);
            end loop;
            if L.Cur in Comment_Or_Line_End then
               End_Line (L);
               return False;
            end if;
            return Line_Indentation (L, T);
      end case;
   end Line_Start;

   procedure Check_Indicator_Char (L : in out Lexer; Kind : Token;
                                   T : out Token) is
   begin
      if Scalars.Next_Is_Plain_Safe (L) then
         Scalars.Read_Plain_Scalar (L);
         T := Scalar;
      else
         L.Token_Start := L.Pos;
         T := Kind;
         L.Cur := Next (L);
         L.State := Before_Indentation_Setting_Token'Access;
      end if;
   end Check_Indicator_Char;

   subtype Flow_Depth_Modifier is Integer range -1 .. 1;

   procedure Handle_Flow_Indicator (L : in out Lexer;
                                    Modifier : Flow_Depth_Modifier) is
   begin
      L.Token_Start := L.Pos;
      L.Flow_Depth := L.Flow_Depth + Modifier;
      case Modifier is
         when -1 =>
            if L.Flow_Depth = 0 then
               L.Json_Enabling_State := After_Token'Access;
            end if;
            L.State := L.Json_Enabling_State;
         when 1 =>
            if L.Flow_Depth = 1 then
               L.Json_Enabling_State := After_Json_Enabling_Token'Access;
            end if;
            L.State := After_Token'Access;
         when others =>
            L.State := After_Token'Access;
      end case;
   end Handle_Flow_Indicator;

   function Inside_Line (L : in out Lexer; T : out Token) return Boolean is
   begin
      case L.Cur is
         when ':' =>
            Check_Indicator_Char (L, Map_Value_Ind, T);
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
            Scalars.Read_Double_Quoted_Scalar (L);
            L.State := L.Json_Enabling_State;
            return True;
         when ''' =>
            Scalars.Read_Single_Quoted_Scalar (L);
            L.State := L.Json_Enabling_State;
            return True;
         when '>' | '|' =>
            raise Lexer_Error with "Not implemented: block scalars";
         when '{' =>
            Handle_Flow_Indicator (L, 1);
            T := Flow_Map_Start;
            return True;
         when '}' =>
            Handle_Flow_Indicator (L, -1);
            T := Flow_Map_End;
            return True;
         when '[' =>
            Handle_Flow_Indicator (L, 1);
            T := Flow_Seq_Start;
            return True;
         when ']' =>
            Handle_Flow_Indicator (L, -1);
            T := Flow_Seq_End;
            return True;
         when ',' =>
            Handle_Flow_Indicator (L, 0);
            T := Flow_Separator;
            return True;
         when '!' =>
            raise Lexer_Error with "Not implemented: tag handles";
         when '&' =>
            raise Lexer_Error with "Not implemented: anchors";
         when '*' =>
            raise Lexer_Error with "Not implemented: aliases";
         when '@' =>
            raise Lexer_Error with "Not implemented: attributes";
         when '`' =>
            raise Lexer_Error with
              "Reserved characters cannot start a plain scalar.";
         when others =>
            Scalars.Read_Plain_Scalar (L);
            T := Scalar;
            return True;
      end case;
   end Inside_Line;

   function Indentation_Setting_Token (L : in out Lexer; T : out Token)
                                       return Boolean is
      Cached_Indentation : constant Natural := L.Pos - L.Line_Start - 1;
   begin
      return Ret : constant Boolean := Inside_Line (L, T) do
         if Ret then
            L.Indentation := Cached_Indentation;
         end if;
      end return;
   end Indentation_Setting_Token;

   function After_Token (L : in out Lexer; T : out Token) return Boolean is
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

   function Before_Indentation_Setting_Token (L : in out Lexer; T : out Token)
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

   function After_Json_Enabling_Token (L : in out Lexer; T : out Token)
                                       return Boolean is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      case L.Cur is
         when ':' =>
            L.Token_Start := L.Pos;
            T := Map_Value_Ind;
            L.Cur := Next (L);
            L.State := After_Token'Access;
            return True;
         when Comment_Or_Line_End =>
            End_Line (L);
            return False;
         when others =>
            L.State := Inside_Line'Access;
            return False;
      end case;
   end After_Json_Enabling_Token;

   function Line_Indentation (L : in out Lexer; T : out Token)
                              return Boolean is
   begin
      T := Indentation;
      L.State := Indentation_Setting_Token'Access;
      return True;
   end Line_Indentation;

   function Line_Dir_End (L : in out Lexer; T : out Token)
                          return Boolean is
   begin
      T := Directives_End;
      L.State := Inside_Line'Access;
      L.Indentation := -1;
      return True;
   end Line_Dir_End;

   --  similar to Indentation_After_Plain_Scalar, but used for a document end
   --  marker ending a plain scalar.
   function Line_Doc_End (L : in out Lexer; T : out Token)
                          return Boolean is
   begin
      T := Document_End;
      L.State := Expect_Line_End'Access;
      L.Line_Start_State := Outside_Doc'Access;
      return True;
   end Line_Doc_End;

end Yada.Lexing;
