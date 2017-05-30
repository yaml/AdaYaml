with Ada.Unchecked_Deallocation;

package body Yada.Lexing is
   use Ada.Strings.Maps;
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
         while not Is_In (L.Buffer.all (Peek), Line_Ends) loop
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
                 new String (1 .. 2 * L.Buffer'Last);
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
   end Handle_CR;

   procedure Handle_LF (L : in out Lexer) is
   begin
      if L.Pos = L.Sentinel then
         Refill_Buffer (L);
         L.Pos := 1;
      end if;
      L.Line_Start := L.Pos;
      L.Cur_Line := L.Cur_Line + 1;
   end Handle_LF;

   function New_Lexer (Input : Sources.Source_Access; Buffer : Buffer_Type)
     return Lexer is
      (Lexer'(Input => Input, Sentinel => Buffer.all'Last + 1, Buffer => Buffer,
              Pos => Buffer.all'First, Indentation => <>, Cur_Line => 1,
              State => Outside_Doc'Access, Cur => <>,
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
        New_Lexer (null, new String (1 .. Input'Length)) do
         L.Buffer.all := Input;
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

   --  this function escapes a given string by converting all non-printable
   --  characters plus '"', ''' and '\', into c-style backslash escape
   --  sequences. it also surrounds the string with double quotation marks.
   --  this is primarily used for error message rendering
   function Escape (S : String) return String is
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
   end Escape;

   function Escape (C : Character) return String is (Escape ("" & C));

   function Next_Token (L : in out Lexer) return Token is
      Ret : Token;
   begin
      loop
         exit when L.State.all (L, Ret);
      end loop;
      return Ret;
   end Next_Token;

   procedure Tell_Indentation (L : in out Lexer; Indentation : Natural) is
   begin
      L.Indentation := Indentation;
   end Tell_Indentation;

   function Short_Lexeme (L : Lexer) return String is
      (L.Buffer (L.Token_Start .. L.Pos - 1));

   function Full_Lexeme (L : Lexer) return String is
     (L.Buffer (L.Token_Start - 1 .. L.Pos - 1));

   procedure Finish_Line (L : in out Lexer; Next_State : Lexer_State) is
   begin
      while L.Cur = ' ' loop
         L.Cur := Next (L);
      end loop;
      case L.Cur is
         when '#' =>
            loop
               L.Cur := Next (L);
               exit when Is_In (L.Cur, Space_Or_Line_End);
            end loop;
         when Line_Feed | Carriage_Return | End_Of_Input =>
            null;
         when others =>
            raise Lexer_Error with
              "Unexpected character (expected line end): " & Escape (L.Cur);
      end case;
      case L.Cur is
         when Line_Feed =>
            Handle_LF (L);
            L.State := Next_State;
         when Carriage_Return =>
            Handle_CR (L);
            L.State := Next_State;
         when End_Of_Input =>
            L.State := Stream_End'Access;
         when others => null; --  can not happen at this point
      end case;
   end Finish_Line;


   function Outside_Doc (L : in out Lexer; T : out Token) return Boolean is
   begin
      case L.Cur is
         when '%' =>
            L.Token_Start := L.Pos;
            loop
               L.Cur := Next (L);
               exit when Is_In (L.Cur, Space_Or_Line_End);
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
            null;
            return False;
         when '.' =>
            null;
            return False;
         when others =>
            L.Indentation := 0;
            while L.Cur = ' ' loop
               L.Cur := Next (L);
               L.Indentation := L.Indentation + 1;
            end loop;
            if Is_In (L.Cur, Comment_Or_Line_End) then
               Finish_Line (L, Outside_Doc'Access);
               return False;
            end if;
            T := Indentation;
            L.State := Inside_Line'Access;
            return True;
      end case;
   end Outside_Doc;


   function Yaml_Version (L : in out Lexer; T : out Token) return Boolean is
      (False);
   function Tag_Shorthand (L : in out Lexer; T : out Token) return Boolean is
      (False);
   function Tag_Uri (L : in out Lexer; T : out Token) return Boolean is
      (False);
   function Unknown_Directive (L : in out Lexer; T : out Token) return Boolean
     is (False);
   function Stream_End (L : in out Lexer; T : out Token) return Boolean is
      (False);
   function Inside_Line (L : in out Lexer; T : out Token) return Boolean is
      (False);
end Yada.Lexing;
