--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with AUnit.Assertions; use AUnit.Assertions;

package body Yaml.Dom.Dumping.Test is
   use type Text.Reference;

   procedure Register_Tests (T : in out TC) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Plain_Scalar_Document'Access, "Plain scalar document");
      Register_Routine (T, Quoted_Scalar_Document'Access, "Quoted scalar document");
      Register_Routine (T, Explicit_Document'Access, "Explicit document");
   end Register_Tests;

   procedure Set_Up (T : in out TC) is
   begin
      T.Pool.Create (8092);
   end Set_Up;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Dumping tests for DOM");
   end Name;

   type Event_Array is array (Positive range <>) of Event;

   function To_String (Value : Properties) return String is
     ("{anchor: " & Value.Anchor & ", tag: " & Value.Tag & "}");

   procedure Assert_Equal (Expected, Actual : Event) is
   begin
      Assert (Expected.Kind = Actual.Kind, "Event kinds differ (expected: " &
                Expected.Kind'Img & ", actual: " & Actual.Kind'Img & ")");
      case Expected.Kind is
         when Document_Start =>
            Assert (Expected.Implicit_Start = Actual.Implicit_Start,
                    "implicit start differs (expected: " &
                      Expected.Implicit_Start'Img & ", actual: " &
                      Actual.Implicit_Start'Img);
         when Document_End =>
            Assert (Expected.Implicit_End = Actual.Implicit_End,
                    "implicit end differs (expected: " &
                      Expected.Implicit_End'Img & ", actual: " &
                      Actual.Implicit_End'Img);
         when Stream_Start | Stream_End | Annotation_End | Sequence_End |
              Mapping_End => null;
         when Mapping_Start | Sequence_Start =>
            Assert (Expected.Collection_Style = Actual.Collection_Style,
                    "styles differ (expected: " & Expected.Collection_Style'Img &
                      ", actual: " & Actual.Collection_Style'Img & ")");
            Assert (Expected.Collection_Properties = Actual.Collection_Properties,
                    "properties differ (expected: " &
                      To_String (Expected.Collection_Properties) & ", actual: " &
                      To_String (Actual.Collection_Properties) & ")");
         when Scalar =>
            Assert (Expected.Scalar_Style = Actual.Scalar_Style,
                    "styles differ (expected: " & Expected.Scalar_Style'Img &
                      ", actual: " & Actual.Scalar_Style'Img & ")");
            Assert (Expected.Scalar_Properties = Actual.Scalar_Properties,
                    "properties differ (expected: " &
                      To_String (Expected.Scalar_Properties) & ", actual: " &
                      To_String (Actual.Scalar_Properties) & ")");
            Assert (Expected.Content = Actual.Content,
                    "content differs (expected: " & Expected.Content.Value.Data.all &
                      ", actual: " & Actual.Content.Value.Data.all & ")");
         when Annotation_Start =>
            Assert (Expected.Annotation_Properties = Actual.Annotation_Properties,
                    "properties differ (expected: " &
                      To_String (Expected.Annotation_Properties) & ", actual: " &
                      To_String (Actual.Annotation_Properties) & ")");
            Assert (Expected.Name = Actual.Name,
                    "name differs (expected: " & Expected.Name.Value.Data.all &
                      ", actual: " & Actual.Name.Value.Data.all & ")");
         when Alias =>
            Assert (Expected.Target = Actual.Target,
                    "name differs (expected: " & Expected.Target.Value.Data.all &
                      ", actual: " & Actual.Target.Value.Data.all & ")");
      end case;
   end Assert_Equal;

   procedure Assert_Equals (Doc : Document_Reference; Expected : Event_Array;
                            Implicit_Start, Implicit_End : Boolean := True) is
      Actual : constant Events.Queue.Reference := To_Event_Queue (Doc);
      Expected_Index : Positive := Expected'First;
      Cur : Event;
   begin
      Assert (Actual.Value.Length > 2,
              "actual event sequence does not contain expected stream start / document start events");
      Cur := Actual.Value.First;
      Assert_Equal ((Kind => Stream_Start, others => <>), Cur);
      Actual.Value.Dequeue;
      Cur := Actual.Value.First;
      Assert_Equal ((Kind => Document_Start, Implicit_Start => Implicit_Start,
                     others => <>), Cur);
      Actual.Value.Dequeue;
      while Expected_Index <= Expected'Last loop
         declare
            Cur : constant Event := Actual.Value.First;
         begin
            Assert (Expected_Index <= Expected'Last,
                    "Actual serialization contains more events than expected, first unexpected event: " &
                      Cur.Kind'Img);
            Assert_Equal (Expected (Expected_Index), Cur);
            Expected_Index := Expected_Index + 1;
         end;
         Actual.Value.Dequeue;
      end loop;
      Assert (Actual.Value.Length = 2,
              "actual event sequence does not contain expected document end / stream end events");
      Cur := Actual.Value.First;
      Assert_Equal ((Kind => Document_End, Implicit_End => Implicit_End,
                     others => <>), Cur);
      Actual.Value.Dequeue;
      Cur := Actual.Value.First;
      Assert_Equal ((Kind => Stream_End, others => <>), Cur);
      Actual.Value.Dequeue;
   end Assert_Equals;

   procedure Plain_Scalar_Document (T : in out Test_Cases.Test_Case'Class) is
      Doc : constant Document_Reference := New_Document (TC (T).Pool);
   begin
      Doc.Set_Root (Doc.New_Scalar ("scalar"));
      Assert_Equals (Doc, (1 => (Kind => Scalar, Content => TC (T).Pool.From_String ("scalar"),
                                 Scalar_Properties => (Tag => Tags.Question_Mark, Anchor => <>),
                           others => <>)));
   end Plain_Scalar_Document;

   procedure Quoted_Scalar_Document (T : in out Test_Cases.Test_Case'Class) is
      Doc : constant Document_Reference := New_Document (TC (T).Pool);
   begin
      Doc.Set_Root (Doc.New_Scalar ("quoted scalar",
                    Style => Double_Quoted));
      Assert_Equals (Doc, (1 => (Kind => Scalar, Content => TC (T).Pool.From_String ("quoted scalar"),
                                 Scalar_Style => Double_Quoted,
                                 Scalar_Properties => (Tag => Tags.Question_Mark, Anchor => <>),
                                 others => <>)));
   end Quoted_Scalar_Document;

   procedure Explicit_Document (T : in out Test_Cases.Test_Case'Class) is
      Doc : constant Document_Reference := New_Document (TC (T).Pool,
                                                         False, False);
   begin
      Doc.Set_Root (Doc.New_Scalar ("explicit document",
                    Style => Single_Quoted));
      Assert_Equals (Doc, (1 => (Kind => Scalar, Content => TC (T).Pool.From_String ("explicit document"),
                                 Scalar_Style => Single_Quoted,
                                 Scalar_Properties => (Tag => Tags.Question_Mark, Anchor => <>),
                                 others => <>)), False, False);
   end Explicit_Document;
end Yaml.Dom.Dumping.Test;
