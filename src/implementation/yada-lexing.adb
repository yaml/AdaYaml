with Ada.Unchecked_Deallocation;

package body Yada.Lexing is
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
         while not Ada.Strings.Maps.Is_In (L.Buffer.all (Peek), Line_Ends) loop
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
   end Handle_CR;

   procedure Handle_LF (L : in out Lexer) is
   begin
      if L.Pos = L.Sentinel then
         Refill_Buffer (L);
         L.Pos := 1;
      end if;
   end Handle_LF;

   function From_Source
     (Input : Sources.Source_Access;
      Initial_Buffer_Size : Positive := Default_Initial_Buffer_Size)
      return Lexer is
   begin
      return L : Lexer :=
        Lexer'(Input => Input, Sentinel => Initial_Buffer_Size + 1, Pos => 1,
               Buffer => new String (1 .. Initial_Buffer_Size)) do
         Refill_Buffer (L);
      end return;
   end From_Source;
end Yada.Lexing;
