--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events is
   use Yaml.Strings;

   function Is_Empty (Props : Events.Properties) return Boolean is
     ((Props.Anchor = Null_Content and then Props.Tag = Null_Content and then
       Events.Content_Stacks.Length (Props.Annotations) = 0));

   function To_String (E : Event) return String is
      function Ann_String (Ann : Content_Stacks.Stack; Start : Positive := 1)
                           return String is
        (if Start > Content_Stacks.Length (Ann) then "" else
              " @" & Content_Stacks.Element (Ann, Start).Get &
           Ann_String (Ann, Start + 1));

      function Prop_String (A : Properties) return String is
        ((if A.Anchor = Null_Content then "" else " &" & A.Anchor.Get) &
         (if A.Tag = Null_Content then "" else " <" & A.Tag.Get & '>') &
         Ann_String (A.Annotations));

      function Scalar_Indicator (S : Scalar_Style_Type) return String is
        ((case S is
             when Plain | Any => " :",
             when Single_Quoted => " '",
             when Double_Quoted => " """,
             when Literal => " |",
             when Folded => " >"));

      function Escaped (C : Content) return String is
         Ret : String (1 .. UTF_8_String'(C.Get)'Length * 2);
         Pos : Positive := 1;
      begin
         for I in UTF_8_String'(C.Get)'Range loop
            case UTF_8_String'(C.Get) (I) is
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
                  Ret (Pos) := UTF_8_String'(C.Get) (I);
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
              Scalar_Indicator (E.Scalar_Style) & Escaped (E.Value);
         when Alias =>
            return "=ALI *" & E.Target.Get;
      end case;
   end To_String;

end Yaml.Events;
