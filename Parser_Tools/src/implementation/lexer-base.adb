--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Lexer.Base is
   procedure Free is new Ada.Unchecked_Deallocation
     (String, Buffer_Type);

   subtype Line_End is Character with Static_Predicate =>
     Line_End in Line_Feed | Carriage_Return | End_Of_Input;

   procedure Init (Object : in out Instance; Input : Source.Pointer;
                   Initial_Buffer_Size : Positive :=
                     Default_Initial_Buffer_Size) is
   begin
      Object.Internal.Input := Input;
      Object.Buffer := new String (1 .. Initial_Buffer_Size);
      Object.Internal.Sentinel := Initial_Buffer_Size + 1;
      Refill_Buffer (Object);
   end Init;

   procedure Init (Object : in out Instance; Input : String) is
   begin
      Object.Internal.Input := null;
      Object.Buffer := new String (1 .. Input'Length + 1);
      Object.Internal.Sentinel := Input'Length + 2;
      Object.Buffer.all := Input & End_Of_Input;
   end Init;

   function Next (Object : in out Instance) return Character is
   begin
      return C : constant Character := Object.Buffer (Object.Pos) do
         Object.Pos := Object.Pos + 1;
      end return;
   end Next;

   procedure Refill_Buffer (L : in out Instance) is
      Bytes_To_Copy : constant Natural := L.Buffer'Last + 1 - L.Internal.Sentinel;
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
         L.Internal.Sentinel := Peek + 1;
         return True;
      end Search_Sentinel;
   begin
      if Bytes_To_Copy > 0 then
         L.Buffer (1 .. Bytes_To_Copy) :=
           L.Buffer (L.Internal.Sentinel .. L.Buffer'Last);
      end if;
      loop
         L.Internal.Input.Read_Data
           (L.Buffer (Fill_At .. L.Buffer'Last), Bytes_Read);
         if Bytes_Read < L.Buffer'Last - Fill_At then
            L.Internal.Sentinel := Fill_At + Bytes_Read + 1;
            L.Buffer (L.Internal.Sentinel - 1) := End_Of_Input;
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

   procedure Handle_CR (L : in out Instance) is
   begin
      if L.Buffer (L.Pos) = Line_Feed then
         L.Pos := L.Pos + 1;
      else
         raise Lexer_Error with "pure CR line breaks not allowed.";
      end if;
      L.Prev_Lines_Chars :=
        L.Prev_Lines_Chars + L.Pos - L.Line_Start;
      if L.Pos = L.Internal.Sentinel then
         Refill_Buffer (L);
         L.Pos := 1;
      end if;
      L.Line_Start := L.Pos;
      L.Cur_Line := L.Cur_Line + 1;
   end Handle_CR;

   procedure Handle_LF (L : in out Instance) is
   begin
      L.Prev_Lines_Chars :=
        L.Prev_Lines_Chars + L.Pos - L.Line_Start;
      if L.Pos = L.Internal.Sentinel then
         Refill_Buffer (L);
         L.Pos := 1;
      end if;
      L.Line_Start := L.Pos;
      L.Cur_Line := L.Cur_Line + 1;
   end Handle_LF;

   procedure Finalize (Object : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Source.Instance'Class, Source.Pointer);
      use type Source.Pointer;
   begin
      if Object.Internal.Input /= null then
         Free (Object.Internal.Input);
      end if;
   end Finalize;
end Lexer.Base;
