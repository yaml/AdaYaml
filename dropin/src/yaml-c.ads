--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with System;
with Interfaces.C.Strings;
with Yaml.Events;
with Yaml.Strings;
private with Yaml.Parsing;
private with Yaml.Presenting;

package Yaml.C is
   --  this is an implementation of libyaml's C interface declared in yaml.h

   function Get_Version_String return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "yaml_get_version_string";

   procedure Get_Version (Major, Minor, Patch : out Interfaces.C.int)
     with Export, Convention => C, External_Name => "yaml_get_version";

   type Encoding_Type is
     (Any, UTF8, UTF16LE, UTF16BE) with Convention => C;

   type Bool is new Boolean with Convention => C;
   for Bool'Size use Interfaces.C.int'Size;

   type Error_Type is
     (No_Error, Memory_Error, Reader_Error, Scanner_Error, Parser_Error,
      Composer_Error, Writer_Error, Emitter_Error) with Convention => C;

   type Event_Type is
     (No_Event, Stream_Start, Stream_End, Document_Start, Document_End,
      Alias, Scalar, Sequence_Start, Sequence_End, Mapping_Start,
      Mapping_End) with Convention => C;

   type Event_Data (T : Event_Type := No_Event) is record
      case T is
         when Stream_Start =>
            Encoding : Encoding_Type;
         when Document_Start =>
            --  TODO: make available from parser
            Version_Directive, Start_Dir, End_Dir : System.Address;
            DS_Implicit : Bool;
         when Document_End =>
            DE_Implicit : Bool;
         when Alias =>
            Ali_Anchor : Strings.Exported_String;
         when Scalar =>
            Scalar_Anchor, Scalar_Tag, Value : Strings.Exported_String;
            Length : Interfaces.C.size_t;
            Plain_Implicit, Quoted_Implicit : Bool;
            Scalar_Style : Events.Scalar_Style_Type;
         when Sequence_Start =>
            Seq_Anchor, Seq_Tag : Strings.Exported_String;
            Seq_Implicit : Bool;
            Seq_Style : Events.Collection_Style_Type;
         when Mapping_Start =>
            Map_Anchor, Map_Tag : Strings.Exported_String;
            Map_Implicit : Bool;
            Map_Style : Events.Collection_Style_Type;
         when others => null;
      end case;
   end record with Unchecked_Union, Convention => C;

   type Event is record
      Kind : Event_Type;
      Data : Event_Data;
      Start_Mark, End_Mark : Mark;
   end record with Convention => C;

   type Event_Access is access Event with Convention => C;

   function Stream_Start_Event_Initialize (E : out Event;
                                           Encoding : Encoding_Type) return Bool
      with Export, Convention => C,
     External_Name => "yaml_stream_start_event_initialize";

   function Stream_End_Event_Initialize (E : out Event) return Bool with Export,
     Convention => C, External_Name => "yaml_stream_end_event_initialize";

   function Document_Start_Event_Initialize
     (E : out Event; Version_Directive, Tag_Directive_Start, Tag_Directive_End :
      System.Address; Implicit : Bool) return Bool with Export, Convention => C,
     External_Name => "yaml_document_start_event_initialize";

   function Document_End_Event_Initialize
     (E : out Event; Implicit : Bool) return Bool with Export, Convention => C,
     External_Name => "yaml_document_end_event_initialize";

   function Alias_Event_Initialize
     (E : out Event; Anchor : Interfaces.C.Strings.chars_ptr) return Bool with
     Export, Convention => C, External_Name => "yaml_alias_event_initialize";

   function Scalar_Event_Initialize
     (E : out Event; Anchor, Tag, Value : Interfaces.C.Strings.chars_ptr;
      Plain_Implicit, Quoted_Implicit : Bool; Style : Events.Scalar_Style_Type)
      return Bool with Export, Convention => C,
     External_Name => "yaml_scalar_event_initialize";

   function Sequence_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Events.Collection_Style_Type) return Bool with
     Export, Convention => C,
     External_Name => "yaml_sequence_start_event_initialize";

   function Sequence_End_Event_Initialize
     (E : out Event) return Bool with Export, Convention => C,
     External_Name => "yaml_sequence_end_event_initialize";

   function Mapping_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Events.Collection_Style_Type) return Bool with
     Export, Convention => C,
     External_Name => "yaml_mapping_start_event_initialize";

   function Mapping_End_Event_Initialize
     (E : out Event) return Bool with Export, Convention => C,
     External_Name => "yaml_mapping_end_event_initialize";

   procedure Event_Delete (E : in out Event) with Export, Convention => C,
     External_Name => "yaml_event_delete";

   type Parser is limited private;

   function Parser_Initialize (P : in out Parser) return Bool with Export,
     Convention => C, External_Name => "yaml_parser_initialize";

   procedure Parser_Delete (P : in out Parser) with Export, Convention => C,
     External_Name => "yaml_parser_delete";

   procedure Parser_Set_Input_String (P : in out Parser;
                                      Input : Interfaces.C.Strings.chars_ptr;
                                      Size : Interfaces.C.size_t) with Export,
     Convention => C, External_Name => "yaml_parser_set_input_string";

   --  yaml_parser_set_input_file must be implemented in C since there is no
   --  Ada type to map C's FILE type to.

   function Parser_Parse (P : in out Parser; E : out Event) return Bool with
     Export, Convention => C, External_Name => "yaml_parser_parse";

   type Emitter is limited private;

   function Emitter_Initialize (Em : in out Emitter) return Bool with Export,
     Convention => C, External_Name => "yaml_emitter_initialize";

   procedure Emitter_Delete (Em : in out Emitter) with Export, Convention => C,
     External_Name => "yaml_emitter_delete";

   procedure Emitter_Set_Output (Em : in out Emitter; Output : System.Address;
                                 Size : Interfaces.C.size_t;
                                 Size_Written : access Interfaces.C.size_t) with
     Export, Convention => C, External_Name => "yaml_emitter_set_output_string";

   function Emitter_Emit (Em : in out Emitter; E : in out Event) return Bool
     with Export, Convention => C, External_Name => "yaml_emitter_emit";
private
   type Parser_Holder is record
      P : Parsing.Parser;
   end record;

   type Parser is access Parser_Holder;

   type Emitter_Holder is record
      E : Presenting.Presenter;
   end record;

   type Emitter is access Emitter_Holder;
end Yaml.C;
