--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text;

package Yaml is
   --  occurs when the lexical analysis of a YAML character streams discovers
   --  ill-formed input.
   Lexer_Error : exception;

   --  occurs when the syntactic analysis of a YAML token stream discovers an
   --  ill-formed input.
   Parser_Error : exception;

   --  occurs when an ill-formed event stream is tried to be presented.
   Presenter_Error : exception;

   --  occurs when data cannot be written to a destination.
   Destination_Error : exception;

   --  all positions in a mark start at 1
   subtype Mark_Position is Positive;

   --  a position in the input stream.
   type Mark is record
      Index, Line, Column : Mark_Position;
   end record with Convention => C;

   --  the version of the library. major and minor version correspond to the
   --  YAML version, the patch version is local to this implementation.
   function Version_Major return Natural with Inline;
   function Version_Minor return Natural with Inline;
   function Version_Patch return Natural with Inline;


   type Event_Kind is (Stream_Start, Stream_End, Document_Start, Document_End,
                       Alias, Scalar, Sequence_Start, Sequence_End,
                       Mapping_Start, Mapping_End);
   type Collection_Style_Type is (Any, Block, Flow) with
     Convention => C;
   type Scalar_Style_Type is
     (Any, Plain, Single_Quoted, Double_Quoted, Literal, Folded) with
     Convention => C;
   subtype Flow_Scalar_Style_Type is Scalar_Style_Type range Literal .. Folded;

   type Properties is record
      Anchor, Tag : Text.Reference := Text.Empty;
   end record;

   function Is_Empty (Props : Properties) return Boolean with Inline;

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
end Yaml;
