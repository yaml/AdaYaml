--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Yaml.Destination.C_String;
with Text.Pool;

package body Yaml.C is
   Creation_Pool : Text.Pool.Reference;

   Version_String : constant Interfaces.C.Strings.chars_ptr :=
     Interfaces.C.Strings.New_String
       (Ada.Strings.Fixed.Trim (Version_Major'Img, Ada.Strings.Left) & '.' &
            Ada.Strings.Fixed.Trim (Version_Minor'Img, Ada.Strings.Left) & '.' &
            Ada.Strings.Fixed.Trim (Version_Patch'Img, Ada.Strings.Left));

   function Get_Version_String return Interfaces.C.Strings.chars_ptr is
      (Version_String);

   procedure Get_Version (Major, Minor, Patch : out Interfaces.C.int) is
   begin
      Major := Interfaces.C.int (Version_Major);
      Minor := Interfaces.C.int (Version_Minor);
      Patch := Interfaces.C.int (Version_Patch);
   end Get_Version;

   function Stream_Start_Event_Initialize (E : out Event;
                                           Encoding : Encoding_Type)
                                           return Bool is
   begin
      E.Kind := Stream_Start;
      E.Data.Encoding := Encoding;
      return True;
   end Stream_Start_Event_Initialize;

   function Stream_End_Event_Initialize (E : out Event) return Bool is
   begin
      E.Kind := Stream_End;
      return True;
   end Stream_End_Event_Initialize;

   function Document_Start_Event_Initialize
     (E : out Event; Version_Directive, Tag_Directive_Start, Tag_Directive_End :
      System.Address; Implicit : Bool) return Bool is
   begin
      E := (Kind => Document_Start, Data =>
              (T => Document_Start, Version_Directive => Version_Directive,
               Start_Dir => Tag_Directive_Start, DS_Implicit => Implicit,
               End_Dir => Tag_Directive_End), others => <>);
      return True;
   end Document_Start_Event_Initialize;

   function Document_End_Event_Initialize
     (E : out Event; Implicit : Bool) return Bool is
   begin
      E := (Kind => Document_End, Data =>
              (T => Document_End, DE_Implicit => Implicit), others => <>);
      return True;
   end Document_End_Event_Initialize;

   function Alias_Event_Initialize
     (E : out Event; Anchor : Interfaces.C.Strings.chars_ptr) return Bool is
   begin
      E := (Kind => Alias, Data => (T => Alias, Ali_Anchor => Text.Export
                                    (Creation_Pool.From_String (
                                       Interfaces.C.Strings.Value (Anchor)))),
           others => <>);
      return True;
   end Alias_Event_Initialize;

   function Scalar_Event_Initialize
     (E : out Event; Anchor, Tag, Value : Interfaces.C.Strings.chars_ptr;
      Plain_Implicit, Quoted_Implicit : Bool; Style : Scalar_Style_Type)
      return Bool is
      Converted_Value : constant Standard.String :=
        Interfaces.C.Strings.Value (Value);
   begin
      E := (Kind => Scalar, Data => (T => Scalar,
                                     Scalar_Tag => Text.Export
                                       (Creation_Pool.From_String (
                                        Interfaces.C.Strings.Value (Tag))),
                                     Scalar_Anchor => Text.Export
                                       (Creation_Pool.From_String (
                                        Interfaces.C.Strings.Value (Anchor))),
                                     Value => Text.Export
                                       (Creation_Pool.From_String (
                                        Converted_Value)),
                                     Length => Converted_Value'Length,
                                     Plain_Implicit => Plain_Implicit,
                                     Quoted_Implicit => Quoted_Implicit,
                                     Scalar_Style => Style), others => <>);
      return True;
   end Scalar_Event_Initialize;

   function Sequence_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Collection_Style_Type) return Bool is
   begin
      E := (Kind => Sequence_Start, Data =>
              (T => Sequence_Start, Seq_Anchor => Text.Export
               (Creation_Pool.From_String (Interfaces.C.Strings.Value (Anchor))),
               Seq_Tag => Text.Export (Creation_Pool.From_String (
                 Interfaces.C.Strings.Value (Tag))),
               Seq_Implicit => Implicit, Seq_Style => Style), others => <>);
      return True;
   end Sequence_Start_Event_Initialize;

   function Sequence_End_Event_Initialize (E : out Event) return Bool is
   begin
      E := (Kind => Sequence_End, Data => (T => Sequence_End), others => <>);
      return True;
   end Sequence_End_Event_Initialize;

   function Mapping_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Collection_Style_Type) return Bool is
   begin
      E := (Kind => Mapping_Start, Data =>
              (T => Mapping_Start, Map_Anchor => Text.Export
               (Creation_Pool.From_String (
                  Interfaces.C.Strings.Value (Anchor))),
               Map_Tag => Text.Export (Creation_Pool.From_String (
                 Interfaces.C.Strings.Value (Tag))),
               Map_Implicit => Implicit, Map_Style => Style), others => <>);
      return True;
   end Mapping_Start_Event_Initialize;

   function Mapping_End_Event_Initialize (E : out Event) return Bool is
   begin
      E := (Kind => Mapping_End, Data => (T => Mapping_End), others => <>);
      return True;
   end Mapping_End_Event_Initialize;

   procedure Event_Delete (E : in out Event) is
      pragma Unmodified (E);
   begin
      case E.Kind is
         when Scalar =>
            Text.Delete_Exported (E.Data.Scalar_Anchor);
            Text.Delete_Exported (E.Data.Scalar_Tag);
            Text.Delete_Exported (E.Data.Value);
         when Mapping_Start =>
            Text.Delete_Exported (E.Data.Map_Anchor);
            Text.Delete_Exported (E.Data.Map_Tag);
         when Sequence_Start =>
            Text.Delete_Exported (E.Data.Seq_Anchor);
            Text.Delete_Exported (E.Data.Seq_Tag);
         when Alias =>
            Text.Delete_Exported (E.Data.Ali_Anchor);
         when others => null;
      end case;
   end Event_Delete;

   function Parser_Initialize (P : in out Parser_Type) return Bool is
   begin
      P := new Parser_Holder;
      return True;
   end Parser_Initialize;

   procedure Parser_Delete (P : in out Parser_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Holder, Parser_Type);
   begin
      Free (P);
   end Parser_Delete;

   procedure Parser_Set_Input_String (P : in out Parser_Type;
                                      Input : Interfaces.C.Strings.chars_ptr;
                                      Size : Interfaces.C.size_t) is
   begin
      P.Instance.Set_Input (Interfaces.C.Strings.Value (Input, Size));
   end Parser_Set_Input_String;

   function To_C (M : Mark) return C_Mark is
     ((Index  => Interfaces.C.size_t (M.Index),
       Line   => Interfaces.C.size_t (M.Line),
       Column => Interfaces.C.size_t (M.Column)));

   function To_Ada (C : C_Mark) return Mark is
     ((Index  => Mark_Position (C.Index),
       Line   => Mark_Position (C.Line),
       Column => Mark_Position (C.Column)));

   function Parser_Parse (P : in out Parser_Type; E : out Event) return Bool is
      Raw : constant Yaml.Event := P.Instance.Next;
      function To_Type return Event_Type is
        (case Raw.Kind is
            when Stream_Start => Stream_Start,
            when Stream_End => Stream_End,
            when Document_Start => Document_Start,
            when Document_End => Document_End,
            when Mapping_Start => Mapping_Start,
            when Mapping_End => Mapping_End,
            when Sequence_Start => Sequence_Start,
            when Sequence_End => Sequence_End,
            when Scalar => Scalar,
            when Alias => Alias,
            when Annotation_Start => Annotation_Start,
            when Annotation_End => Annotation_End);
      function To_Data return Event_Data is
        (case Raw.Kind is
            when Stream_Start => (T => Stream_Start, Encoding => UTF8),
            when Stream_End => (T => Stream_End),
            when Document_Start => (T => Document_Start,
                                           Version_Directive => System.Null_Address,
                                           Start_Dir => System.Null_Address,
                                           End_Dir => System.Null_Address,
                                           DS_Implicit => Bool (Raw.Implicit_Start)),
            when Document_End => (T => Document_End,
                                         DE_Implicit => Bool (Raw.Implicit_End)),
            when Mapping_Start => (T => Mapping_Start,
                                          Map_Anchor => Text.Export (Raw.Collection_Properties.Anchor),
                                          Map_Tag => Text.Export (Raw.Collection_Properties.Tag),
                                          Map_Implicit => False,
                                          Map_Style => Raw.Collection_Style),
            when Mapping_End => (T => Mapping_End),
            when Sequence_Start => (T => Sequence_Start,
                                          Seq_Anchor => Text.Export (Raw.Collection_Properties.Anchor),
                                          Seq_Tag => Text.Export (Raw.Collection_Properties.Tag),
                                          Seq_Implicit => False,
                                          Seq_Style => Raw.Collection_Style),
            when Sequence_End => (T => Sequence_End),
            when Scalar => (T => Scalar,
                                   Scalar_Anchor => Text.Export (Raw.Scalar_Properties.Anchor),
                                   Scalar_Tag => Text.Export (Raw.Scalar_Properties.Tag),
                                   Value => Text.Export (Raw.Content),
                                   Length => Interfaces.C.size_t (Raw.Content.Length),
                                   Plain_Implicit => False,
                                   Quoted_Implicit => False,
                                   Scalar_Style => Raw.Scalar_Style),
            when Alias => (T => Alias,
                           Ali_Anchor => Text.Export (Raw.Target)),
            when Annotation_Start => (T => Annotation_Start,
                                      Ann_Anchor => Text.Export (Raw.Annotation_Properties.Anchor),
                                      Ann_Tag    => Text.Export (Raw.Annotation_Properties.Tag),
                                      Ann_Name   => Text.Export (Raw.Name)),
            when Annotation_End => (T => Annotation_End));

   begin
      E := (Kind => To_Type, Data => To_Data,
            Start_Mark => To_C (Raw.Start_Position),
            End_Mark => To_C (Raw.End_Position));
      return True;
   end Parser_Parse;

   function Emitter_Initialize (Em : in out Emitter) return Bool is
   begin
      Em := new Emitter_Holder;
      return True;
   end Emitter_Initialize;

   procedure Emitter_Delete (Em : in out Emitter) is
      procedure Free is new Ada.Unchecked_Deallocation (Emitter_Holder, Emitter);
   begin
      Free (Em);
   end Emitter_Delete;

   procedure Emitter_Set_Output (Em : in out Emitter; Output : System.Address;
                                 Size : Interfaces.C.size_t;
                                 Size_Written : access Interfaces.C.size_t) is
   begin
      Em.E.Set_Output (Destination.C_String.As_Destination
                       (Output, Size, Size_Written));
   end Emitter_Set_Output;

   function Emitter_Emit (Em : in out Emitter; E : in out Event) return Bool is
      function To_Properties (Tag, Anchor : Text.Exported)
                              return Properties is
        ((Anchor => Text.Import (Anchor), Tag => Text.Import (Tag)));

      function To_Event (E : Event) return Yaml.Event is
        (case E.Kind is
            when Stream_Start => (Kind => Stream_Start,
                                  Start_Position => To_Ada (E.Start_Mark),
                                  End_Position => To_Ada (E.End_Mark)),
            when Stream_End => (Kind => Stream_End,
                                Start_Position => To_Ada (E.Start_Mark),
                                End_Position => To_Ada (E.End_Mark)),
            when Document_Start => (Kind => Document_Start,
                                    Start_Position => To_Ada (E.Start_Mark),
                                    End_Position => To_Ada (E.End_Mark),
                                    Version => Text.Empty,
                                    Implicit_Start => Boolean (E.Data.DS_Implicit)),
            when Document_End => (Kind => Document_End,
                                  Start_Position => To_Ada (E.Start_Mark),
                                  End_Position => To_Ada (E.End_Mark),
                                  Implicit_End => Boolean (E.Data.DE_Implicit)),
            when Mapping_Start =>
           (Kind => Mapping_Start,
            Start_Position => To_Ada (E.Start_Mark),
            End_Position => To_Ada (E.End_Mark),
            Collection_Style => E.Data.Map_Style,
            Collection_Properties => To_Properties (E.Data.Map_Tag, E.Data.Map_Anchor)),
            when Mapping_End => (Kind => Mapping_End,
                                 Start_Position => To_Ada (E.Start_Mark),
                                 End_Position => To_Ada (E.End_Mark)),
            when Sequence_Start =>
           (Kind => Sequence_Start,
            Start_Position => To_Ada (E.Start_Mark),
            End_Position => To_Ada (E.End_Mark),
            Collection_Style => E.Data.Seq_Style,
            Collection_Properties => To_Properties (E.Data.Seq_Tag, E.Data.Seq_Anchor)),
            when Sequence_End => (Kind => Sequence_End,
                                  Start_Position => To_Ada (E.Start_Mark),
                                  End_Position => To_Ada (E.End_Mark)),
            when Scalar => (Kind => Scalar,
                            Start_Position => To_Ada (E.Start_Mark),
                            End_Position => To_Ada (E.End_Mark),
                            Scalar_Properties => To_Properties (E.Data.Scalar_Tag, E.Data.Scalar_Anchor),
                            Content => Text.Import (E.Data.Value),
                            Scalar_Style => E.Data.Scalar_Style),
            when Alias => (Kind => Alias,
                           Start_Position => To_Ada (E.Start_Mark),
                           End_Position => To_Ada (E.End_Mark),
                           Target => Text.Import (E.Data.Ali_Anchor)),
            when Annotation_Start => (Kind => Annotation_Start,
                                      Start_Position => To_Ada (E.Start_Mark),
                                      End_Position => To_Ada (E.End_Mark),
                                      Annotation_Properties => To_Properties (E.Data.Ann_Tag, E.Data.Ann_Anchor),
                                      Name => Text.Import (E.Data.Ann_Name)),
            when Annotation_End => (Kind => Annotation_End,
                                    Start_Position => To_Ada (E.Start_Mark),
                                    End_Position => To_Ada (E.End_Mark)),
            when No_Event => (others => <>));
   begin
      if E.Kind /= No_Event then
         Em.E.Put (To_Event (E));
      end if;
      Event_Delete (E);
      return True;
   end Emitter_Emit;
begin
   Creation_Pool.Create (8192);
end Yaml.C;
