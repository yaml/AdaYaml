with AUnit.Assertions; use AUnit.Assertions;

with Ada.Text_IO;
with Yada.Sources.Files;

package body Yada.Lexing.Buffering_Test is
   procedure Register_Tests (T : in out TC) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_File_Without_Refill'Access, "Read file without refill");
      Register_Routine
        (T, Test_File_With_Single_Refill'Access, "Read file with single refill");
   end Register_Tests;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Buffering tests for Lexer");
   end Name;

   procedure Test_File_Without_Refill (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : constant Sources.Source_Access := Sources.Files.As_Source ("test/data/64char.yaml");
      L : Lexer := From_Source (S, 64);
      C : Character;
   begin
      Ada.Text_IO.Put_Line ("Buffer = """ & L.Buffer.all & """");
      Assert (L.Buffer.all'Length = 64, "Buffer length does not match! Val:" & L.Buffer.all'Length'Img);
      Assert (L.Buffer (1 .. 5) = "Lorem", "Buffer contains wrong data!");
   end Test_File_Without_Refill;

   procedure Test_File_With_Single_Refill (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : constant Sources.Source_Access := Sources.Files.As_Source ("test/data/98char.yaml");
      L : Lexer := From_Source (S, 64);
      C : Character;
   begin
      Assert (L.Buffer.all'Length = 64, "Buffer length does not match! Val: " & L.Buffer.all'Length'Img);
      Assert (L.Buffer (1 .. 5) = "Lorem", "Buffer contains wrong data!");
   end Test_File_With_Single_Refill;
end Yada.Lexing.Buffering_Test;
