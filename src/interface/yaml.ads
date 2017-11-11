--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;
with Text;
with Lexer;

package Yaml is
   --  occurs when the lexical analysis of a YAML character streams discovers
   --  ill-formed input.
   Lexer_Error : exception renames Lexer.Lexer_Error;

   --  occurs when the syntactic analysis of a YAML token stream discovers an
   --  ill-formed input.
   Parser_Error : exception;

   --  occurs when a DOM cannot be composed from a given event stream.
   Composer_Error : exception;

   --  occurs when an ill-formed event stream is tried to be presented.
   Presenter_Error : exception;

   --  occurs when data cannot be written to a destination.
   Destination_Error : exception;

   --  occurs when an event stream contains an invalid sequence of events.
   Stream_Error : exception;

   --  occurs when annotation processing encounters an invalid usage of an
   --  annotation.
   Annotation_Error : exception;

   --  the version of the library. major and minor version correspond to the
   --  YAML version, the patch version is local to this implementation.
   function Version_Major return Natural with Inline;
   function Version_Minor return Natural with Inline;
   function Version_Patch return Natural with Inline;


   --  all positions in a mark start at 1
   subtype Mark_Position is Positive;

   --  a position in the input stream.
   type Mark is record
      Index, Line, Column : Mark_Position;
   end record;

   type Event_Kind is (Stream_Start, Stream_End, Document_Start, Document_End,
                       Alias, Scalar, Sequence_Start, Sequence_End,
                       Mapping_Start, Mapping_End, Annotation_Start,
                       Annotation_End);
   type Collection_Style_Type is (Any, Block, Flow) with
     Convention => C;
   type Scalar_Style_Type is
     (Any, Plain, Single_Quoted, Double_Quoted, Literal, Folded) with
     Convention => C;
   subtype Flow_Scalar_Style_Type is Scalar_Style_Type range Literal .. Folded;

   type Properties is record
      Anchor, Tag : Text.Reference;
   end record;

   function Default_Properties return Properties;

   function Is_Empty (Props : Properties) return Boolean with Inline;

   type Event (Kind : Event_Kind := Stream_End) is record
      --  Start_Position is first character, End_Position is after last
      --  character. this is necessary for zero-length events.
      Start_Position, End_Position : Mark;
      case Kind is
         when Document_Start =>
            Version : Text.Reference;
            Implicit_Start : Boolean := True;
         when Document_End =>
            Implicit_End : Boolean;
         when Mapping_Start | Sequence_Start =>
            Collection_Style : Collection_Style_Type := Any;
            Collection_Properties : Properties;
         when Annotation_Start =>
            Annotation_Properties : Properties;
            Namespace : Text.Reference;
            Name : Text.Reference;
         when Scalar =>
            Scalar_Properties : Properties;
            Content : Text.Reference;
            Scalar_Style : Scalar_Style_Type := Any;
         when Alias =>
            Target : Text.Reference;
         when Mapping_End | Sequence_End | Annotation_End | Stream_Start |
              Stream_End => null;
      end case;
   end record;

   function To_String (E : Event) return String;

   Standard_Annotation_Namespace : constant Text.Reference;

   --  base type for refcounted types (mainly event streams). all streams and
   --  some other objects can be used with reference-counting smart pointers, so
   --  this base type implements the reference counting. note that this type
   --  does not have any stream semantic; that is to be implemented by child
   --  types by providing a Stream_Concept instance (if they are streams).
   --
   --  beware that this type is only the vessel for the reference count and does
   --  not do any reference counting itself; the reference-counting management
   --  functions must be called from a smart pointer type. An object of a child
   --  type can be used on the stack, in which case the reference count is not
   --  used and instead the object just goes out of scope.
   type Refcount_Base is abstract limited new
     Ada.Finalization.Limited_Controlled with private;

   --  increases reference count. only call this explicitly when implementing
   --  a reference-counting smart pointer.
   procedure Increase_Refcount (Object : not null access Refcount_Base'Class);

   --  decreases reference count. only call this explicitly when implementing a
   --  reference-counting smart pointer. this procedure will free the object
   --  when the reference count hits zero, rendering the provided pointer
   --  useless and dangerous to use afterwards!
   procedure Decrease_Refcount (Object : not null access Refcount_Base'Class);
private
   Standard_Annotation_Namespace_Holder : constant Text.Constant_Instance :=
     Text.Hold ("@@");

   Standard_Annotation_Namespace : constant Text.Reference := Text.Held
     (Standard_Annotation_Namespace_Holder);

   type Refcount_Base is abstract limited new
     Ada.Finalization.Limited_Controlled with record
      Refcount : Natural := 1;
   end record;
end Yaml;
