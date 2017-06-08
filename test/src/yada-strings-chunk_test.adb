with Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;

package body Yada.Strings.Chunk_Test is
   procedure Register_Tests (T : in out TC) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_One_String'Access, "One string");
      Register_Routine
        (T, Test_Two_Strings'Access, "Two strings");
   end Register_Tests;

   procedure Set_Up (T : in out TC) is
   begin
      Initialize (T.Pool, 128);
   end Set_Up;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Chunk tests for String_Pool");
   end Name;

   procedure Test_One_String (T : in out Test_Cases.Test_Case'Class) is
      Test_Data : constant String := "123456";
      C : constant Content := From_String (TC (T).Pool, Test_Data);
   begin
      Ada.Text_IO.Put_Line ("Test one string, chunk content:");
      Ada.Text_IO.Put_Line (Current_Chunk_As_String (TC (T).Pool));
      Assert (Value (C) = Test_Data, "Data mismatch!");
   end Test_One_String;

   procedure Test_Two_Strings (T : in out Test_Cases.Test_Case'Class) is
      S1 : constant String := "a";
      S2 : constant String := "b";
      C1 : constant Content := From_String (TC (T).Pool, S1);
      C2 : constant Content := From_String (TC (T).Pool, S2);
   begin
      Assert (Value (C1) = S1, "S1 mismatch, is " & Value (C1));
      Assert (Value (C2) = S2, "S2 mismatch!");
   end Test_Two_Strings;

end Yada.Strings.Chunk_Test;
