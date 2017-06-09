with AUnit.Assertions; use AUnit.Assertions;

with Yada.Sources.Files;

with Utils;

package body Yada.Lexing.Buffering_Test is
   procedure Register_Tests (T : in out TC) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_File_Without_Refill'Access, "Read file of exact buffer size");
      Register_Routine
        (T, Test_File_With_Single_Refill'Access, "Read file exceeding buffer size");
   end Register_Tests;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Buffering tests for Lexer");
   end Name;

   procedure Set_Up (T : in out TC) is
   begin
      Strings.Create (T.Pool, 8092);
   end Set_Up;

   procedure Test_File_Without_Refill (T : in out Test_Cases.Test_Case'Class) is
      Data_Path : constant String := "test/data/64char.yaml";
      Expected : constant String := Utils.File_Content (Data_Path) & End_Of_Input;

      S : constant Sources.Source_Access := Sources.Files .As_Source (Data_Path);
      L : Lexer;
   begin
      Lexing.Init (L, S, TC (T).Pool, 64);
      Assert (L.Buffer.all'Length = 64, "Buffer length does not match! Val:" & L.Buffer.all'Length'Img);
      for I in Expected'Range loop
         Assert (Expected (I) = L.Cur, "Buffer contents at" & I'Img &
                   " does not match. Expected """ & Expected (I) & """, got """ &
                   L.Cur & """.");
         exit when L.Cur = End_Of_Input;
         case L.Cur is
            when Line_Feed => Handle_LF (L);
            when Carriage_Return => Handle_CR (L);
            when others => L.Cur := Next (L);
         end case;
      end loop;
   end Test_File_Without_Refill;

   procedure Test_File_With_Single_Refill (T : in out Test_Cases.Test_Case'Class) is
      Data_Path : constant String := "test/data/98char.yaml";
      Expected : constant String := Utils.File_Content (Data_Path);

      S : constant Sources.Source_Access := Sources.Files.As_Source (Data_Path);
      L : Lexer;
   begin
      Lexing.Init (L, S, TC (T).Pool, 64);
      Assert (L.Buffer.all'Length = 64, "Buffer length does not match! Val: " & L.Buffer.all'Length'Img);
      for I in Expected'Range loop
         Assert (Expected (I) = L.Cur, "Buffer contents at" & I'Img &
                   " does not match. Expected """ & Expected (I) & """, got """ &
                   L.Cur & """.");
         exit when L.Cur = End_Of_Input;
         case L.Cur is
            when Line_Feed => Handle_LF (L);
            when Carriage_Return => Handle_CR (L);
            when others => L.Cur := Next (L);
         end case;
      end loop;
   end Test_File_With_Single_Refill;
end Yada.Lexing.Buffering_Test;
