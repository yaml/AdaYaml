--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Yaml.Destination.C_Handler;
with Yaml.Destination.C_String;
with Yaml.Source;
with Yaml.Tags;
with Lexer.Source.C_Handler;
with Text.Pool;

package body Yaml.C is
   use type System.Address;
   use type Interfaces.C.size_t;
   use type Interfaces.C.Strings.chars_ptr;
   use type Text.Reference;

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

   --  token API not implemented.
   procedure Token_Delete (Token : System.Address) is null;

   procedure Init (E : out Event; Kind : Event_Type) is
   begin
      E.Start_Mark := (Index => 1, Line => 1, Column => 1);
      E.End_Mark := (Index => 1, Line => 1, Column => 1);
      E.Kind := Kind;
   end Init;

   function Stream_Start_Event_Initialize (E : out Event;
                                           Encoding : Encoding_Type)
                                           return Bool is
   begin
      Init (E, Stream_Start);
      E.Data.Encoding := Encoding;
      return True;
   end Stream_Start_Event_Initialize;

   function Stream_End_Event_Initialize (E : out Event) return Bool is
   begin
      Init (E, Stream_End);
      return True;
   end Stream_End_Event_Initialize;

   function Document_Start_Event_Initialize
     (E : out Event; Version_Directive, Tag_Directive_Start, Tag_Directive_End :
      System.Address; Implicit : Bool) return Bool is
   begin
      Init (E, Document_Start);
      E.Data := (T => Document_Start, Version_Directive => Version_Directive,
                 Start_Dir => Tag_Directive_Start, DS_Implicit => Implicit,
                 End_Dir => Tag_Directive_End);
      return True;
   end Document_Start_Event_Initialize;

   function Document_End_Event_Initialize
     (E : out Event; Implicit : Bool) return Bool is
   begin
      Init (E, Document_End);
      E.Data := (T => Document_End, DE_Implicit => Implicit);
      return True;
   end Document_End_Event_Initialize;

   function Alias_Event_Initialize
     (E : out Event; Anchor : Interfaces.C.Strings.chars_ptr) return Bool is
   begin
      Init (E, Alias);
      E.Data := (T => Alias, Ali_Anchor => Text.Export
                 (Creation_Pool.From_String (
                    Interfaces.C.Strings.Value (Anchor))));
      return True;
   end Alias_Event_Initialize;

   function Ada_Value_For (C_Value : Interfaces.C.Strings.chars_ptr;
                           Default : Text.Reference := Text.Empty)
                           return Text.Reference is
     ((if C_Value = Interfaces.C.Strings.Null_Ptr then Default else
            Creation_Pool.From_String (Interfaces.C.Strings.Value (C_Value))));

   function Scalar_Event_Initialize
     (E : out Event; Anchor, Tag, Value : Interfaces.C.Strings.chars_ptr;
      Plain_Implicit, Quoted_Implicit : Bool; Style : Scalar_Style_Type)
      return Bool is
      Converted_Value : constant Standard.String :=
        Interfaces.C.Strings.Value (Value);
   begin
      Init (E, Scalar);
      E.Data := (T => Scalar,
                 Scalar_Tag => Text.Export (Ada_Value_For (Tag, Tags.Question_Mark)),
                 Scalar_Anchor => Text.Export (Ada_Value_For (Anchor)),
                 Value => Text.Export (Creation_Pool.From_String (
                   Converted_Value)),
                 Length => Converted_Value'Length,
                 Plain_Implicit => Plain_Implicit,
                 Quoted_Implicit => Quoted_Implicit,
                 Scalar_Style => Style);
      return True;
   end Scalar_Event_Initialize;

   function Sequence_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Collection_Style_Type) return Bool is
   begin
      Init (E, Sequence_Start);
      E.Data := (T => Sequence_Start, Seq_Anchor => Text.Export
                 (Ada_Value_For (Anchor)),
                 Seq_Tag => Text.Export (Ada_Value_For (Tag, Tags.Question_Mark)),
                 Seq_Implicit => Implicit, Seq_Style => Style);
      return True;
   end Sequence_Start_Event_Initialize;

   function Sequence_End_Event_Initialize (E : out Event) return Bool is
   begin
      Init (E, Sequence_End);
      return True;
   end Sequence_End_Event_Initialize;

   function Mapping_Start_Event_Initialize
     (E : out Event; Anchor, Tag : Interfaces.C.Strings.chars_ptr;
      Implicit : Bool; Style : Collection_Style_Type) return Bool is
   begin
      Init (E, Mapping_Start);
      E.Data := (T => Mapping_Start, Map_Anchor => Text.Export
                 (Ada_Value_For (Anchor)),
                 Map_Tag => Text.Export (Ada_Value_For (Tag, Tags.Question_Mark)),
                 Map_Implicit => Implicit, Map_Style => Style);
      return True;
   end Mapping_Start_Event_Initialize;

   function Mapping_End_Event_Initialize (E : out Event) return Bool is
   begin
      Init (E, Mapping_End);
      return True;
   end Mapping_End_Event_Initialize;

   procedure Event_Delete (E : in out Event) is
      pragma Unmodified (E);

      procedure Delete_If_Exists (Value : Text.Exported) is
      begin
         if Value /= System.Null_Address then
            Text.Delete_Exported (Value);
         end if;
      end Delete_If_Exists;
   begin
      case E.Kind is
         when Scalar =>
            Delete_If_Exists (E.Data.Scalar_Anchor);
            Delete_If_Exists (E.Data.Scalar_Tag);
            Text.Delete_Exported (E.Data.Value);
         when Mapping_Start =>
            Delete_If_Exists (E.Data.Map_Anchor);
            Delete_If_Exists (E.Data.Map_Tag);
         when Sequence_Start =>
            Delete_If_Exists (E.Data.Seq_Anchor);
            Delete_If_Exists (E.Data.Seq_Tag);
         when Alias =>
            Text.Delete_Exported (E.Data.Ali_Anchor);
         when others => null;
      end case;
   end Event_Delete;

   function Document_Initialize (Document, Version_Directive,
                                 Tag_Directives_Start, Tag_Directives_End :
                                 System.Address; Start_Implicit, End_Implicit :
                                 Bool) return Bool is (False);

   procedure Document_Delete (Document : System.Address) is null;

   function Document_Get_Node (Document : System.Address;
                               Index : Interfaces.C.int) return System.Address
   is (System.Null_Address);

   function Document_Get_Root_Node (Document : System.Address)
                                    return System.Address is
     (System.Null_Address);

   function Document_Add_Scalar (Document : System.Address;
                                 Tag, Value : Interfaces.C.Strings.chars_ptr;
                                 Length : Interfaces.C.int;
                                 Style : Scalar_Style_Type) return Bool is
     (False);

   function Document_Add_Sequence (Document : System.Address;
                                   Tag : Interfaces.C.Strings.chars_ptr;
                                   Style : Collection_Style_Type) return Bool is
     (False);

   function Document_Add_Mapping (Document : System.Address;
                                  Tag : Interfaces.C.Strings.chars_ptr;
                                  Style : Collection_Style_Type) return Bool is
     (False);

   function Document_Append_Sequence_Item (Document : System.Address;
                                           Sequence, Item : Interfaces.C.int)
                                           return Bool is (False);

   function Document_Append_Mapping_Pair
     (Document : System.Address; Mapping, Key, Value : Interfaces.C.int)
      return Bool is (False);

   procedure Parser_Set_Encoding (P : in out Parser_Type;
                                  Encoding : Encoding_Type) is null;

   function Parser_Initialize (P : in out Parser_Type) return Bool is
   begin
      P.Error := No_Error;
      P.Problem := Interfaces.C.Strings.Null_Ptr;
      P.Ptr := new Parser.Instance;
      return True;
   exception
      when E : Storage_Error =>
         P.Error := Memory_Error;
         P.Problem := Interfaces.C.Strings.New_String (Ada.Exceptions.Exception_Message (E));
         return False;
      when E : others =>
         P.Error := Parser_Error;
         P.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
         return False;
   end Parser_Initialize;

   procedure Parser_Delete (P : in out Parser_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser.Instance, Parser_Pointer);
   begin
      if P.Problem /= Interfaces.C.Strings.Null_Ptr then
         Interfaces.C.Strings.Free (P.Problem);
      end if;
      Free (P.Ptr);
   end Parser_Delete;

   procedure Parser_Set_Input_String (P : in out Parser_Type;
                                      Input : Interfaces.C.Strings.chars_ptr;
                                      Size : Interfaces.C.size_t) is
   begin
      P.Ptr.Set_Input (Interfaces.C.Strings.Value (Input, Size));
   end Parser_Set_Input_String;

   function fread (Ptr : System.Address; Size, Count : Interfaces.C.size_t;
                   Stream : System.Address) return Interfaces.C.size_t with
     Import, Convention => C, External_Name => "fread";

   function fwrite (Ptr : System.Address; Size, Count : Interfaces.C.size_t;
                    Stream : System.Address) return Interfaces.C.size_t with
     Import, Convention => C, External_Name => "fwrite";

   function ferror (Stream : System.Address) return Interfaces.C.int with
     Import, Convention => C, External_Name => "ferror";

   function File_Read_Handler (Data, Buffer : System.Address;
                               Size : Interfaces.C.size_t;
                               Size_Read : out Interfaces.C.size_t) return Bool
     with Convention => C;

   function File_Read_Handler (Data, Buffer : System.Address;
                               Size : Interfaces.C.size_t;
                               Size_Read : out Interfaces.C.size_t) return Bool
   is
      use type Interfaces.C.int;
   begin
      Size_Read := fread (Buffer, 1, Size, Data);
      return Bool (ferror (Data) = 0);
   end File_Read_Handler;

   procedure Parser_Set_Input_File (P : in out Parser_Type;
                                    File : System.Address) is
   begin
      Parser_Set_Input (P, File_Read_Handler'Access, File);
   end Parser_Set_Input_File;

   procedure Parser_Set_Input (P : in out Parser_Type;
                               Handler : Read_Handler; Data : System.Address) is
   begin
      P.Ptr.Set_Input (Source.C_Handler.As_Source (Data, Handler));
   end Parser_Set_Input;

   function Parser_Scan (P : in out Parser_Type; Token : System.Address)
                         return Bool is
      pragma Unreferenced (Token);
   begin
      P.Error := Scanner_Error;
      P.Problem := Interfaces.C.Strings.New_String
        ("AdaYaml does not implement the low-level scanner API");
      return False;
   end Parser_Scan;

   function To_C (M : Mark) return C_Mark is
     ((Index  => Interfaces.C.size_t (M.Index),
       Line   => Interfaces.C.size_t (M.Line),
       Column => Interfaces.C.size_t (M.Column)));

   function To_Ada (C : C_Mark) return Mark is
     ((Index  => Mark_Position (C.Index),
       Line   => Mark_Position (C.Line),
       Column => Mark_Position (C.Column)));

   function Parser_Parse (P : in out Parser_Type; E : out Event) return Bool is
   begin
      --  use internal declare block so that exception handler handles
      --  all possible exceptions
      declare
         Raw : constant Yaml.Event := P.Ptr.Next;
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

         generic
            Null_Value : Text.Reference;
         function Export_Nullable (Value : Text.Reference) return Text.Exported;

         function Export_Nullable (Value : Text.Reference)
                                   return Text.Exported is
           (if Value = Null_Value then System.Null_Address else
               Text.Export (Value));

         function Export_Tag is new Export_Nullable (Tags.Question_Mark);
         function Export_Anchor is new Export_Nullable (Text.Empty);

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
                                      Map_Anchor => Export_Anchor (Raw.Collection_Properties.Anchor),
                                      Map_Tag => Export_Tag (Raw.Collection_Properties.Tag),
                                      Map_Implicit => False,
                                      Map_Style => Raw.Collection_Style),
               when Mapping_End => (T => Mapping_End),
               when Sequence_Start => (T => Sequence_Start,
                                       Seq_Anchor => Export_Anchor (Raw.Collection_Properties.Anchor),
                                       Seq_Tag => Export_Tag (Raw.Collection_Properties.Tag),
                                       Seq_Implicit => False,
                                       Seq_Style => Raw.Collection_Style),
               when Sequence_End => (T => Sequence_End),
               when Scalar => (T => Scalar,
                               Scalar_Anchor => Export_Anchor (Raw.Scalar_Properties.Anchor),
                               Scalar_Tag => Export_Tag (Raw.Scalar_Properties.Tag),
                               Value => Text.Export (Raw.Content),
                               Length => Interfaces.C.size_t (Raw.Content.Length),
                               Plain_Implicit => False,
                               Quoted_Implicit => False,
                               Scalar_Style => Raw.Scalar_Style),
               when Alias => (T => Alias,
                              Ali_Anchor => Text.Export (Raw.Target)),
               when Annotation_Start => (T => Annotation_Start,
                                         Ann_Anchor => Text.Export (Raw.Annotation_Properties.Anchor),
                                         Ann_Tag    => Export_Tag (Raw.Annotation_Properties.Tag),
                                         Ann_Name   => Text.Export (Raw.Name)),
               when Annotation_End => (T => Annotation_End));
      begin
         E := (Kind => To_Type, Data => To_Data,
               Start_Mark => To_C (Raw.Start_Position),
               End_Mark => To_C (Raw.End_Position));
         return True;
      end;
   exception
      when Storage_Error =>
         P.Error := Memory_Error;
         --  do not set problem because if we're out of memory, that would
         --  likely raise another exception.
         return False;
      when E : Lexer_Error =>
         P.Error := Scanner_Error;
         P.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Message (E));
         return False;
      when E : Yaml.Parser_Error =>
         P.Error := Parser_Error;
         P.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Message (E));
         return False;
      when E : others =>
         P.Error := Parser_Error;
         P.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
         return False;
   end Parser_Parse;

   function Parser_Load (P : in out Parser_Type; Document : System.Address)
                         return Bool is
      pragma Unreferenced (Document);
   begin
      P.Error := Composer_Error;
      P.Problem := Interfaces.C.Strings.New_String
        ("AdaYaml does not implement the composer API");
      return False;
   end Parser_Load;

   function Emitter_Initialize (Emitter : in out Emitter_Type) return Bool is
   begin
      Emitter.Ptr := new Presenter.Instance;
      return True;
   end Emitter_Initialize;

   procedure Emitter_Delete (Emitter : in out Emitter_Type) is
      procedure Free is new Ada.Unchecked_Deallocation (Presenter.Instance,
                                                        Presenter_Pointer);
   begin
      if Emitter.Problem /= Interfaces.C.Strings.Null_Ptr then
         Interfaces.C.Strings.Free (Emitter.Problem);
      end if;
      Free (Emitter.Ptr);
   end Emitter_Delete;

   procedure Emitter_Set_Output_String
     (Emitter : in out Emitter_Type; Output : System.Address;
      Size : Interfaces.C.size_t; Size_Written : access Interfaces.C.size_t) is
   begin
      Emitter.Ptr.Set_Output (Destination.C_String.As_Destination
                              (Output, Size, Size_Written));
   end Emitter_Set_Output_String;

   function File_Write_Handler (Data, Buffer : System.Address;
                                Size : Interfaces.C.size_t) return Bool with
     Convention => C;

   function File_Write_Handler (Data, Buffer : System.Address;
                                Size : Interfaces.C.size_t) return Bool is
     (Bool (fwrite (Buffer, 1, Size, Data) = Size));

   procedure Emitter_Set_Output_File
     (Emitter : in out Emitter_Type; File : System.Address) is
   begin
      Emitter.Ptr.Set_Output (Destination.C_Handler.As_Destination
                              (File_Write_Handler'Access, File));
   end Emitter_Set_Output_File;

   procedure Emitter_Set_Output (Emitter : in out Emitter_Type;
                                 Handler : Write_Handler;
                                 Data : System.Address) is
   begin
      Emitter.Ptr.Set_Output (Destination.C_Handler.As_Destination
                              (Handler, Data));
   end Emitter_Set_Output;

   function Emitter_Emit (Emitter : in out Emitter_Type; E : in out Event)
                          return Bool is
   begin
      declare
         function To_Properties (Tag, Anchor : Text.Exported)
                              return Properties is
           ((Anchor => (if Anchor = System.Null_Address then Text.Empty else
                           Text.Import (Anchor)),
             Tag => (if Tag = System.Null_Address then Tags.Question_Mark else
                        Text.Import (Tag))));

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
                                         Namespace => Standard_Annotation_Namespace,
                                         Name => Text.Import (E.Data.Ann_Name)),
               when Annotation_End => (Kind => Annotation_End,
                                       Start_Position => To_Ada (E.Start_Mark),
                                       End_Position => To_Ada (E.End_Mark)),
               when No_Event => (others => <>));
      begin
         if E.Kind /= No_Event then
            Emitter.Ptr.Put (To_Event (E));
         end if;
         Event_Delete (E);
         return True;
      end;
   exception
      when Storage_Error =>
         Emitter.Error := Memory_Error;
         --  do not set problem because if we're out of memory, that would
         --  likely raise another exception.
         return False;
      when E : Constraint_Error =>
         Emitter.Error := Emitter_Error;
         Emitter.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Message (E));
         return False;
      when E : Yaml.Presenter_Error =>
         Emitter.Error := Emitter_Error;
         Emitter.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Message (E));
         return False;
      when E : others =>
         Emitter.Error := Emitter_Error;
         Emitter.Problem := Interfaces.C.Strings.New_String
           (Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
         return False;
   end Emitter_Emit;
begin
   Creation_Pool.Create (Text.Pool.Default_Size);
end Yaml.C;
