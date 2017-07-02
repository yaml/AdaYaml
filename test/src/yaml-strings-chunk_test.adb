--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;

package body Yaml.Strings.Chunk_Test is
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
      Create (T.Pool, 128);
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
      Assert (C.Get = Test_Data, "Data mismatch!");
      declare
         C2 : constant Content := C;
      begin
         Ada.Text_IO.Put_Line ("Range after copy: (" & C2.Get.Data.all'First'Img &
           " .." & C2.Get.Data.all'Last'Img & ')');
      end;
   end Test_One_String;

   procedure Test_Two_Strings (T : in out Test_Cases.Test_Case'Class) is
      S1 : constant String := "aaaa";
      S2 : constant String := "bbbb";
      C1 : constant Content := From_String (TC (T).Pool, S1);
      C2 : constant Content := From_String (TC (T).Pool, S2);
   begin
      Ada.Text_IO.Put_Line ("Test two strings, chunk content:");
      Ada.Text_IO.Put_Line (Current_Chunk_As_String (TC (T).Pool));
      Assert (C1.Get = S1, "S1 mismatch, is " & C1.Get);
      Assert (C2.Get = S2, "S2 mismatch!");
   end Test_Two_Strings;

end Yaml.Strings.Chunk_Test;
