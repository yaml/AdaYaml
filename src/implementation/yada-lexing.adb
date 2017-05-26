with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

package body Yada.Lexing is
   End_Of_Input    : constant := Character'Val (4);
   Line_Feed       : constant := Character'Val (10);
   Carriage_Return : constant := Character'Val (13);

   Line_Ends : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Line_Feed & Carriage_Return);

   procedure Free is new Ada.Unchecked_Deallocation (String, not null access String);

   function Next (Object: in out Lexer) return Character is
   begin
      return Object.Buffer (Object.Pos) do
         Object.Pos := Object.Pos + 1;
      end return;
   end Next;

   procedure Handle_CR (L : in out Lexer) is
   begin
      L.Pos := L.Pos + 1;
      if L.Buffer (L.Pos) = Line_Feed then
         L.Pos := L.Pos + 1;
      end if;
      if L.Pos = L.Sentinel then
         Refill_Buffer (L);
         L.Pos := 0;
      end if;
   end Handle_CR;

   procedure Handle_LF (L : in out Lexer) is
   begin
      L.Pos := L.Pos + 1;
      if L.Pos = L.Sentinel then
         Refill_Buffer (L);
         L.Pos = 0;
      end if;
   end Handle_LF;

   procedure Refill_Buffer (L : in out Lexer) is
      Bytes_To_Copy : constant Integer := L.Buffer'Last - L.Sentinel;
      Num_Chars : Integer;
   begin
      L.Buffer (1..Bytes_To_Copy) :=
        L.Buffer (L.Buffer'Last - Bytes_To_Copy + 1 .. L.Buffer'Last);

      -- TODO: read in new chunk of input, fill Num_Chars

      declare
         Minimum : Natural := 0;
         Peek : Natural := Bytes_To_Copy + Num_Chars;
      begin
         while not Ada.Strings.Maps.Is_In (L.Buffer (Peek), Line_Ends) loop
            if Peek = Minimum then
               Minimum := L.Buffer'Last + 1;
               declare
                  New_Buffer : not null access String :=
                    new String (1..2 * L.Buffer'Last);
               begin
                  New_Buffer.all (L.Buffer'Range) := L.Buffer.all;
                  Free (L.Buffer);
                  L.Buffer := New_Buffer;
               end;
               Peek := L.Buffer'Last;
            else
               Peek := Peek - 1;
            end if;
         end loop;
      end;
   end Refill_Buffer;

   function From_String (S : String) return Lexer is
   begin
      return L : Lexer := Lexer'(Pos => 0, Sentinel => S'Last + 1,
                                 Buffer => new String (S'First..S'Last + 1)) do
         L.Buffer.all := S & End_Of_Input;
      end return;
   end From_String;

   function From_File (File_Name : String) return Lexer is
      use Ada.Text_IO;
   begin
      return L : Lexer :=
        Lexer'(Pos => 0, Sentinel => 8096, Buffer => new String (1..8096),
               Input_At => 0,
               Input_Length => Ada.Directories.Size (File_Name)) do
         Open (L.Input, In_File, File_Name);
         Refill_Buffer (L);
      end return;
   end From_File;
end Yada.Lexing;