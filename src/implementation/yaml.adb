--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;
with Yaml.Tags;

package body Yaml is
   function Version_Major return Natural is (1);
   function Version_Minor return Natural is (3);
   function Version_Patch return Natural is (0);

   use type Text.Reference;

   function Default_Properties return Properties is
     ((Anchor => Text.Empty, Tag => Tags.Question_Mark));

   function Is_Empty (Props : Properties) return Boolean is
     ((Props.Anchor = Text.Empty and then Props.Tag = Tags.Question_Mark));

   function To_String (E : Event) return String is
      function Prop_String (A : Properties) return String is
        ((if A.Anchor = Text.Empty then "" else " &" & A.Anchor.Value) &
         (if A.Tag = Tags.Question_Mark then "" else " <" & A.Tag.Value & '>'));

      function Scalar_Indicator (S : Scalar_Style_Type) return String is
        ((case S is
             when Plain | Any => " :",
             when Single_Quoted => " '",
             when Double_Quoted => " """,
             when Literal => " |",
             when Folded => " >"));

      function Escaped (C : Text.Reference) return String is
         Ret : String (1 .. C.Length * 2);
         Pos : Positive := 1;
      begin
         for I in C.Value.Data'Range loop
            case C.Value.Data (I) is
               when Character'Val (7) =>
                  Ret (Pos .. Pos + 1) := "\a";
                  Pos := Pos + 2;
               when Character'Val (8) =>
                  Ret (Pos .. Pos + 1) := "\b";
                  Pos := Pos + 2;
               when Character'Val (9) =>
                  Ret (Pos .. Pos + 1) := "\t";
                  Pos := Pos + 2;
               when Character'Val (10) =>
                  Ret (Pos .. Pos + 1) := "\n";
                  Pos := Pos + 2;
               when Character'Val (13) =>
                  Ret (Pos .. Pos + 1) := "\r";
                  Pos := Pos + 2;
               when '\' =>
                  Ret (Pos .. Pos + 1) := "\\";
                  Pos := Pos + 2;
               when others =>
                  Ret (Pos) := C.Value.Data (I);
                  Pos := Pos + 1;
            end case;
         end loop;
         return Ret (1 .. Pos - 1);
      end Escaped;
   begin
      case E.Kind is
         when Stream_Start => return "+STR";
         when Stream_End => return "-STR";
         when Document_Start =>
            return "+DOC" & (if E.Implicit_Start then "" else " ---");
         when Document_End =>
            return "-DOC" & (if E.Implicit_End then "" else " ...");
         when Mapping_Start =>
            return "+MAP" & Prop_String (E.Collection_Properties);
         when Mapping_End =>
            return "-MAP";
         when Sequence_Start =>
            return "+SEQ" & Prop_String (E.Collection_Properties);
         when Sequence_End =>
            return "-SEQ";
         when Scalar =>
            return "=VAL" & Prop_String (E.Scalar_Properties) &
              Scalar_Indicator (E.Scalar_Style) & Escaped (E.Content);
         when Alias =>
            return "=ALI *" & E.Target.Value;
         when Annotation_Start =>
            return "+ANN" & Prop_String (E.Annotation_Properties) & ' ' &
              E.Namespace & E.Name;
         when Annotation_End =>
            return "-ANN";
      end case;
   end To_String;

   procedure Increase_Refcount (Object : not null access Refcount_Base'Class) is
   begin
      Object.Refcount := Object.Refcount + 1;
   end Increase_Refcount;

   procedure Decrease_Refcount (Object : not null access Refcount_Base'Class) is
      type Base_Access is access all Refcount_Base'Class;

      Ptr : Base_Access := Base_Access (Object);

      procedure Free is new Ada.Unchecked_Deallocation (Refcount_Base'Class,
                                                        Base_Access);
   begin
      Ptr.Refcount := Ptr.Refcount - 1;
      if Ptr.Refcount = 0 then
         Free (Ptr);
      end if;
   end Decrease_Refcount;
end Yaml;
