--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Yaml.Source.File;
with Yaml.Parser.Stream;
with Yaml.Transformation;
with Yaml.Transformator.Annotation_Processor;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;

package body Yaml.Annotation_Test is
   Tests_Dir : constant String :=
     Compose (Containing_Directory (Containing_Directory
              (Current_Directory)), "annotation-test-suite");

   procedure Register_Tests (T : in out TC) is
      procedure Add_Test (Directory_Entry : Directory_Entry_Type) is
         Title_File : File_Type;
         use AUnit.Test_Cases.Registration;
         Dir_Name : constant String := Simple_Name (Directory_Entry);
      begin
         if Dir_Name'Length = 4 then
            Open (Title_File, In_File,
                  Compose (Full_Name (Directory_Entry), "==="));
            if Exists (Compose (Full_Name (Directory_Entry), "error")) then
               Register_Routine (T, Execute_Error_Test'Access,
                                 '[' & Dir_Name & "] " &
                                   Get_Line (Title_File));
            else
               Register_Routine (T, Execute_Next_Test'Access,
                                 '[' & Dir_Name & "] " & Get_Line (Title_File));
            end if;
            Close (Title_File);
            T.Test_Cases.Append (Simple_Name (Directory_Entry));
         end if;
      end Add_Test;
   begin
      Search (Tests_Dir, "",
              (Directory => True, others => False),
              Add_Test'Access);
      T.Cur := 1;
   end Register_Tests;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Annotation Test Suite");
   end Name;

   package Parser_Transformation is new Transformation (Parser.Stream);

   procedure Execute_Next_Test (T : in out Test_Cases.Test_Case'Class) is
      Test_Dir : constant String :=
        Compose (Tests_Dir, TC (T).Test_Cases.Element (TC (T).Cur));
      P : constant Parser.Reference := Parser.New_Parser;
      Trans : Parser_Transformation.Instance :=
        Parser_Transformation.Transform (P);
      Expected : File_Type;
      Output : Unbounded_String;
   begin
      TC (T).Cur := TC (T).Cur + 1;
      P.Value.Set_Input (Source.File.As_Source (Compose (Test_Dir, "in.yaml")));
      Trans.Append (Transformator.Annotation_Processor.New_Processor (P.Value.Pool));
      Open (Expected, In_File, Compose (Test_Dir, "processed.event"));
      loop
         declare
            Expected_Event : constant String := Get_Line (Expected);
            Actual : constant Event := Trans.Next;
            Actual_Event : constant String := To_String (Actual);
         begin
            if Expected_Event = Actual_Event then
               Append (Output, Actual_Event & Character'Val (10));
            else
               Append (Output, "--- " & Actual_Event & Character'Val (10));
               Append (Output, "+++ " & Expected_Event & Character'Val (10));
               Assert (False, "Actual events do not match expected events:" &
                         Character'Val (10) & Character'Val (10) &
                         To_String (Output));
            end if;
            exit when Actual.Kind = Stream_End;
            if End_Of_File (Expected) then
               Assert (False, "More events generated than expected");
            end if;
         end;
      end loop;
      Close (Expected);
   exception when others =>
         Close (Expected);
         raise;
   end Execute_Next_Test;

   procedure Execute_Error_Test (T : in out Test_Cases.Test_Case'Class) is
      Test_Dir : constant String :=
        Compose (Tests_Dir, TC (T).Test_Cases.Element (TC (T).Cur));
      P : constant Parser.Reference := Parser.New_Parser;
      Trans : Parser_Transformation.Instance :=
        Parser_Transformation.Transform (P);
      Expected_Error : File_Type;
      Output : Unbounded_String;
      Cur : Event;
   begin
      TC (T).Cur := TC (T).Cur + 1;
      P.Value.Set_Input (Source.File.As_Source (Compose (Test_Dir, "in.yaml")));
      Trans.Append (Transformator.Annotation_Processor.New_Processor (P.Value.Pool));
      loop
         Cur := Trans.Next;
         Append (Output, To_String (Cur) & Character'Val (10));
         exit when Cur.Kind = Stream_End;
      end loop;
      Open (Expected_Error, In_File, Compose (Test_Dir, "error"));
      declare
         Expected_Message : constant String :=
           (if End_Of_File (Expected_Error) then "" else
                 Get_Line (Expected_Error));
      begin
         Close (Expected_Error);
         Assert (False, "Parsed without error; expected error: " &
                   Expected_Message & Character'Val (10) & "Output: " &
                Character'Val (10) & Character'Val (10) & To_String (Output));
      end;
   exception when Annotation_Error =>
         null;
   end Execute_Error_Test;


end Yaml.Annotation_Test;
