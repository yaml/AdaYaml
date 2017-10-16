--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with System;
with Interfaces.C.Strings;
private with Yaml.Parser;
private with Yaml.Presenter;

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

   procedure Token_Delete (Token : System.Address) with Export,
     Convention => C, External_Name => "yaml_token_delete";

   type Event_Type is
     (No_Event, Stream_Start, Stream_End, Document_Start, Document_End,
      Alias, Scalar, Sequence_Start, Sequence_End, Mapping_Start,
      Mapping_End, Annotation_Start, Annotation_End) with Convention => C;

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
            Ali_Anchor : Text.Exported;
         when Scalar =>
            Scalar_Anchor, Scalar_Tag, Value : Text.Exported;
            Length : Interfaces.C.size_t;
            Plain_Implicit, Quoted_Implicit : Bool;
            Scalar_Style : Scalar_Style_Type;
         when Sequence_Start =>
            Seq_Anchor, Seq_Tag : Text.Exported;
            Seq_Implicit : Bool;
            Seq_Style : Collection_Style_Type;
         when Mapping_Start =>
            Map_Anchor, Map_Tag : Text.Exported;
            Map_Implicit : Bool;
            Map_Style : Collection_Style_Type;
         when Annotation_Start =>
            Ann_Anchor, Ann_Tag : Text.Exported;
            Ann_Name : Text.Exported;
         when others => null;
      end case;
   end record with Unchecked_Union, Convention => C;

   type C_Mark is record
      Index, Line, Column : Interfaces.C.size_t;
   end record;

   type Event is record
      Kind : Event_Type;
      Data : Event_Data;
      Start_Mark, End_Mark : C_Mark;
   end record with Convention => C;

   type Event_Access is access Event with Convention => C;

   type Read_Handler is access function (Data, Buffer : System.Address;
                                         Size : Interfaces.C.size_t;
                                         Size_Read : out Interfaces.C.size_t)
                                         return Bool with Convention => C;

   type Write_Handler is access function (Data, Buffer : System.Address;
                                          Size : Interfaces.C.size_t)
                                          return Bool with Convention => C;

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
      Plain_Implicit, Quoted_Implicit : Bool; Style : Scalar_Style_Type)
      return Bool with Export, Convention => C,
     External_Name => "yaml_scalar_event_initialize";

   function Sequence_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Collection_Style_Type) return Bool with
     Export, Convention => C,
     External_Name => "yaml_sequence_start_event_initialize";

   function Sequence_End_Event_Initialize
     (E : out Event) return Bool with Export, Convention => C,
     External_Name => "yaml_sequence_end_event_initialize";

   function Mapping_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Collection_Style_Type) return Bool with
     Export, Convention => C,
     External_Name => "yaml_mapping_start_event_initialize";

   function Mapping_End_Event_Initialize
     (E : out Event) return Bool with Export, Convention => C,
     External_Name => "yaml_mapping_end_event_initialize";

   procedure Event_Delete (E : in out Event) with Export, Convention => C,
     External_Name => "yaml_event_delete";

   function Document_Initialize (Document, Version_Directive,
                                 Tag_Directives_Start, Tag_Directives_End :
                                 System.Address; Start_Implicit, End_Implicit :
                                 Bool) return Bool with Export, Convention => C,
     External_Name => "yaml_document_initialize";

   procedure Document_Delete (Document : System.Address) with Export,
     Convention => C, External_Name => "yaml_document_delete";

   function Document_Get_Node (Document : System.Address;
                               Index : Interfaces.C.int) return System.Address
     with Export, Convention => C, External_Name => "yaml_document_get_node";

   function Document_Get_Root_Node (Document : System.Address)
                                    return System.Address with Export,
     Convention => C, External_Name => "yaml_document_get_root_node";

   function Document_Add_Scalar (Document : System.Address;
                                 Tag, Value : Interfaces.C.Strings.chars_ptr;
                                 Length : Interfaces.C.int;
                                 Style : Scalar_Style_Type) return Bool
     with Export, Convention => C, External_Name => "yaml_document_add_scalar";

   function Document_Add_Sequence (Document : System.Address;
                                   Tag : Interfaces.C.Strings.chars_ptr;
                                   Style : Collection_Style_Type) return Bool
     with Export, Convention => C,
     External_Name => "yaml_document_add_sequence";

   function Document_Add_Mapping (Document : System.Address;
                                  Tag : Interfaces.C.Strings.chars_ptr;
                                  Style : Collection_Style_Type) return Bool
     with Export, Convention => C, External_Name => "yaml_document_add_mapping";

   function Document_Append_Sequence_Item (Document : System.Address;
                                           Sequence, Item : Interfaces.C.int)
                                           return Bool with Export,
     Convention => C, External_Name => "yaml_document_append_sequence_item";

   function Document_Append_Mapping_Pair
     (Document : System.Address; Mapping, Key, Value : Interfaces.C.int)
      return Bool with Export, Convention => C,
     External_Name => "yaml_document_append_mapping_pair";

   type Parser_Type is limited private;

   function Parser_Initialize (P : in out Parser_Type) return Bool with Export,
     Convention => C, External_Name => "yaml_parser_initialize";

   procedure Parser_Delete (P : in out Parser_Type) with Export,
     Convention => C, External_Name => "yaml_parser_delete";

   procedure Parser_Set_Input_String (P : in out Parser_Type;
                                      Input : Interfaces.C.Strings.chars_ptr;
                                      Size : Interfaces.C.size_t) with Export,
     Convention => C, External_Name => "yaml_parser_set_input_string";

   procedure Parser_Set_Input_File (P : in out Parser_Type;
                                    File : System.Address) with Export,
     Convention => C, External_Name => "yaml_parser_set_input_file";

   procedure Parser_Set_Input (P : in out Parser_Type;
                               Handler : Read_Handler; Data : System.Address)
     with Export, Convention => C, External_Name => "yaml_parser_set_input";

   procedure Parser_Set_Encoding (P : in out Parser_Type;
                                  Encoding : Encoding_Type) with Export,
     Convention => C, External_Name => "yaml_parser_set_encoding";

   function Parser_Scan (P : in out Parser_Type; Token : System.Address)
                         return Bool with Export, Convention => C,
     External_Name => "yaml_parser_scan";

   function Parser_Parse (P : in out Parser_Type; E : out Event) return Bool
     with Export, Convention => C, External_Name => "yaml_parser_parse";

   function Parser_Load (P : in out Parser_Type; Document : System.Address)
                         return Bool with Export, Convention => C,
     External_Name => "yaml_parser_load";

   type Emitter_Type is limited private;

   function Emitter_Initialize (Emitter : in out Emitter_Type)
                                return Bool with Export, Convention => C,
     External_Name => "yaml_emitter_initialize";

   procedure Emitter_Delete (Emitter : in out Emitter_Type) with Export,
     Convention => C, External_Name => "yaml_emitter_delete";

   procedure Emitter_Set_Output_String
     (Emitter : in out Emitter_Type; Output : System.Address;
      Size : Interfaces.C.size_t; Size_Written : access Interfaces.C.size_t)
     with Export, Convention => C,
     External_Name => "yaml_emitter_set_output_string";

   procedure Emitter_Set_Output_File
     (Emitter : in out Emitter_Type; File : System.Address) with Export,
     Convention => C, External_Name => "yaml_emitter_set_output_file";

   procedure Emitter_Set_Output
     (Emitter : in out Emitter_Type; Handler : Write_Handler;
      Data : System.Address) with Export, Convention => C,
     External_Name => "yaml_emitter_set_output";

   function Emitter_Emit (Emitter : in out Emitter_Type; E : in out Event)
                          return Bool with Export, Convention => C,
     External_Name => "yaml_emitter_emit";
private
   type Parser_Pointer is access Parser.Instance;

   type Parser_Type is limited record
      Error : Error_Type;
      Problem : Interfaces.C.Strings.chars_ptr;
      Ptr : Parser_Pointer;
   end record with Convention => C;

   type Presenter_Pointer is access Presenter.Instance;

   type Emitter_Type is limited record
      Error : Error_Type;
      Problem : Interfaces.C.Strings.chars_ptr;
      Ptr : Presenter_Pointer;
   end record with Convention => C;
end Yaml.C;
