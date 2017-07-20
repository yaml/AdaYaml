--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Text;
with Yaml.Stacks;

package Yaml.Events is
   type Event_Kind is (Stream_Start, Stream_End, Document_Start, Document_End,
                       Alias, Scalar, Sequence_Start, Sequence_End,
                       Mapping_Start, Mapping_End);
   type Collection_Style_Type is (Any, Block, Flow) with
     Convention => C;
   type Scalar_Style_Type is
     (Any, Plain, Single_Quoted, Double_Quoted, Literal, Folded) with
     Convention => C;
   subtype Flow_Scalar_Style_Type is Scalar_Style_Type range Literal .. Folded;

   package Text_Stack is new Stacks (Text.Reference);

   type Properties is record
      Anchor, Tag : Text.Reference := Text.Empty;
      Annotations : Text_Stack.Stack;
   end record;

   function Is_Empty (Props : Events.Properties) return Boolean with Inline;

   type Event (Kind : Event_Kind := Stream_End) is record
      --  Start_Position is first character, End_Position is after last
      --  character. this is necessary for zero-length events.
      Start_Position, End_Position : Mark;
      case Kind is
         when Document_Start =>
            Version : Text.Reference;
            Implicit_Start : Boolean;
         when Document_End =>
            Implicit_End : Boolean;
         when Mapping_Start | Sequence_Start =>
            Collection_Style : Collection_Style_Type;
            Collection_Properties : Properties;
         when Scalar =>
            Scalar_Properties : Properties;
            Content : Text.Reference;
            Scalar_Style : Scalar_Style_Type;
         when Alias =>
            Target : Text.Reference;
         when Mapping_End | Sequence_End | Stream_Start | Stream_End =>
            null;
      end case;
   end record;

   function To_String (E : Event) return String;
end Yaml.Events;
