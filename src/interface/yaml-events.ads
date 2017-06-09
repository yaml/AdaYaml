with Yaml.Strings;
with Yaml.Stacks;

package Yaml.Events is
   type Event_Kind is (Stream_Start, Stream_End, Document_Start, Document_End,
                       Alias, Scalar, Sequence_Start, Sequence_End,
                       Mapping_Start, Mapping_End);
   type Collection_Style_Type is (Block, Flow, Any);
   type Scalar_Style_Type is
     (Plain, Single_Quoted, Double_Quoted, Literal, Folded);

   package Content_Stacks is new Stacks (Strings.Content);

   type Properties is record
      Anchor, Tag : Strings.Content := Strings.Null_Content;
      Annotations : Content_Stacks.Stack;
   end record;

   type Event (Kind : Event_Kind := Stream_End) is record
      --  Start_Position is first character, End_Position is after last
      --  character. this is necessary for zero-length events.
      Start_Position, End_Position : Mark;
      case Kind is
         when Document_Start =>
            Version : Strings.Content;
            Implicit_Start : Boolean;
         when Document_End =>
            Implicit_End : Boolean;
         when Mapping_Start | Sequence_Start =>
            Collection_Style : Collection_Style_Type;
            Collection_Properties : Properties;
         when Scalar =>
            Scalar_Properties : Properties;
            Value : Strings.Content;
            Scalar_Style : Scalar_Style_Type;
         when Alias =>
            Target : Strings.Content;
         when Mapping_End | Sequence_End | Stream_Start | Stream_End =>
            null;
      end case;
   end record;

   function To_String (E : Event) return String;
end Yaml.Events;
