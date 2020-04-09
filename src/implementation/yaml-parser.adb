--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers;
with Text.Builder;
with Yaml.Tags;

package body Yaml.Parser is
   use type Lexer.Token_Kind;
   use type Text.Reference;

   function New_Parser return Reference is
      Ptr : constant not null Instance_Access := new Instance;
   begin
      return Reference'(Ada.Finalization.Controlled with Data => Ptr);
   end New_Parser;

   function Value (Object : Reference) return Accessor is
     ((Data => Object.Data));

   procedure Adjust (Object : in out Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;

   procedure Init (P : in out Instance) with Inline is
   begin
      P.Levels := Level_Stacks.New_Stack (32);
      P.Levels.Push ((State => At_Stream_Start'Access, Indentation => -2));
      P.Pool.Create (Text.Pool.Default_Size);
      Tag_Handle_Sets.Init (P.Tag_Handles, P.Pool, 16);
      P.Header_Props := Default_Properties;
      P.Inline_Props := Default_Properties;
   end Init;

   procedure Set_Input (P : in out Instance; Input : Source.Pointer) is
   begin
      Init (P);
      Lexer.Init (P.L, Input, P.Pool);
   end Set_Input;

   procedure Set_Input (P : in out Instance; Input : String) is
   begin
      Init (P);
      Lexer.Init (P.L, Input, P.Pool);
   end Set_Input;

   procedure Set_Warning_Handler
     (P : in out Instance; Handler : access Warning_Handler'Class) is
   begin
      P.Handler := Handler;
   end Set_Warning_Handler;

   function Next (P : in out Instance) return Event is
   begin
      return E : Event do
         while not P.Levels.Top.State (P, E) loop
            null;
         end loop;
      end return;
   end Next;

   function Pool (P : Instance) return Text.Pool.Reference is (P.Pool);

   procedure Finalize (P : in out Instance) is null;


   function Current_Lexer_Token_Start (P : Instance) return Mark is
     (Lexer.Recent_Start_Mark (P.L));

   function Current_Input_Character (P : Instance) return Mark is
     (Lexer.Cur_Mark (P.L));

   function Recent_Lexer_Token_Start (P : Instance) return Mark is
     (P.Current.Start_Pos);

   function Recent_Lexer_Token_End (P : Instance) return Mark is
     (P.Current.End_Pos);

   -----------------------------------------------------------------------------
   --                   internal utility subroutines
   -----------------------------------------------------------------------------

   procedure Reset_Tag_Handles (P : in out Class) is
   begin
      Tag_Handle_Sets.Clear (P.Tag_Handles);
      pragma Warnings (Off);
      if P.Tag_Handles.Set ("!", P.Pool.From_String ("!")) and
        P.Tag_Handles.Set ("!!",
                           P.Pool.From_String ("tag:yaml.org,2002:"))
      then
         null;
      end if;
      pragma Warnings (On);
   end Reset_Tag_Handles;

   function Parse_Tag (P : in out Class)
                       return Text.Reference is
      use type Ada.Containers.Hash_Type;
      Tag_Handle : constant String := Lexer.Full_Lexeme (P.L);
      Holder : constant access constant Tag_Handle_Sets.Holder :=
        P.Tag_Handles.Get (Tag_Handle, False);
   begin
      if Holder.Hash = 0 then
         raise Parser_Error with
           "Unknown tag handle: " & Tag_Handle;
      end if;
      P.Current := Lexer.Next_Token (P.L);
      if P.Current.Kind /= Lexer.Suffix then
         raise Parser_Error with "Unexpected token (expected tag suffix): " &
           P.Current.Kind'Img;
      end if;
      return P.Pool.From_String (Holder.Value & Lexer.Current_Content (P.L));
   end Parse_Tag;

   function To_Style (T : Lexer.Scalar_Token_Kind)
                      return Scalar_Style_Type is
     (case T is
         when Lexer.Plain_Scalar => Plain,
         when Lexer.Single_Quoted_Scalar => Single_Quoted,
         when Lexer.Double_Quoted_Scalar => Double_Quoted,
         when Lexer.Literal_Scalar => Literal,
         when Lexer.Folded_Scalar => Folded) with Inline;

   -----------------------------------------------------------------------------
   --                        state implementations
   -----------------------------------------------------------------------------

   function At_Stream_Start (P : in out Class;
                             E : out Event) return Boolean is
   begin
      P.Levels.Top.all := (State => At_Stream_End'Access, Indentation => -2);
      P.Levels.Push ((State => Before_Doc'Access, Indentation => -1));
      E := Event'(Kind => Stream_Start,
                         Start_Position => (Line => 1, Column => 1, Index => 1),
                         End_Position => (Line => 1, Column => 1, Index => 1));
      P.Current := Lexer.Next_Token (P.L);
      Reset_Tag_Handles (P);
      return True;
   end At_Stream_Start;

   function At_Stream_End (P : in out Class;
                           E : out Event) return Boolean is
      T : constant Lexer.Token := Lexer.Next_Token (P.L);
   begin
      E := Event'(Kind => Stream_End,
                         Start_Position => T.Start_Pos,
                         End_Position => T.End_Pos);
      return True;
   end At_Stream_End;

   function Before_Doc (P : in out Class;
                         E : out Event) return Boolean is
      Version : Text.Reference := Text.Empty;
      Seen_Directives : Boolean := False;
   begin
      loop
         case P.Current.Kind is
         when Lexer.Document_End =>
            if Seen_Directives then
               raise Parser_Error with "Directives must be followed by '---'";
            end if;
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Directives_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position => P.Current.End_Pos,
                        Kind => Document_Start,
                        Implicit_Start => False,
                        Version => Version);
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Top.State := Before_Doc_End'Access;
            P.Levels.Push ((State => After_Directives_End'Access,
                            Indentation => -1));
            return True;
         when Lexer.Stream_End =>
            P.Levels.Pop;
            return False;
         when Lexer.Indentation =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind => Document_Start,
                        Implicit_Start => True,
                        Version => Version);
            P.Levels.Top.State := Before_Doc_End'Access;
            P.Levels.Push ((State => Before_Implicit_Root'Access,
                            Indentation => -1));
            return True;
         when Lexer.Yaml_Directive =>
            Seen_Directives := True;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind /= Lexer.Directive_Param then
               raise Parser_Error with
                 "Invalid token (expected YAML version string): " &
                 P.Current.Kind'Img;
            elsif Version /= Text.Empty then
               raise Parser_Error with
                 "Duplicate YAML directive";
            end if;
            Version := P.Pool.From_String (Lexer.Full_Lexeme (P.L));
            if Version /= "1.3" and then P.Handler /= null then
               P.Handler.Wrong_Yaml_Version (Version.Value);
            end if;
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Tag_Directive =>
            Seen_Directives := True;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind /= Lexer.Tag_Handle then
               raise Parser_Error with
                 "Invalid token (expected tag handle): " & P.Current.Kind'Img;
            end if;
            declare
               Tag_Handle : constant String := Lexer.Full_Lexeme (P.L);
               Holder : access Tag_Handle_Sets.Holder;
            begin
               P.Current := Lexer.Next_Token (P.L);
               if P.Current.Kind /= Lexer.Suffix then
                  raise Parser_Error with
                    "Invalid token (expected tag URI): " & P.Current.Kind'Img;
               end if;
               if Tag_Handle = "!" or Tag_Handle = "!!" then
                  Holder := Tag_Handle_Sets.Get (P.Tag_Handles, Tag_Handle, False);
                  Holder.Value := Lexer.Current_Content (P.L);
               else
                  if not Tag_Handle_Sets.Set (P.Tag_Handles, Tag_Handle,
                                              Lexer.Current_Content (P.L)) then
                     raise Parser_Error with
                       "Redefinition of tag handle " & Tag_Handle;
                  end if;
               end if;
            end;
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Unknown_Directive =>
            Seen_Directives := True;
            if P.Handler /= null then
               declare
                  Name : constant String := Lexer.Short_Lexeme (P.L);
                  Params : Text.Builder.Reference := Text.Builder.Create (P.Pool);
                  First : Boolean := True;
               begin
                  loop
                     P.Current := Lexer.Next_Token (P.L);
                     exit when P.Current.Kind /= Lexer.Directive_Param;
                     if First then
                        First := False;
                     else
                        Params.Append (' ');
                     end if;
                     Params.Append (Lexer.Full_Lexeme (P.L));
                  end loop;
                  P.Handler.Unknown_Directive (Name, Params.Lock.Value.Data.all);
               end;
            else
               loop
                  P.Current := Lexer.Next_Token (P.L);
                  exit when P.Current.Kind /= Lexer.Directive_Param;
               end loop;
            end if;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected directive or document start): " &
              P.Current.Kind'Img;
         end case;
      end loop;
   end Before_Doc;

   function After_Directives_End (P : in out Class;
                                  E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Inline_Start := P.Current.Start_Pos;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                              Indentation => <>));
            return False;
         when Lexer.Indentation =>
            P.Header_Start := P.Inline_Start;
            P.Levels.Top.State := At_Block_Indentation'Access;
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            return False;
         when Lexer.Document_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Scalar,
                               Scalar_Properties => P.Inline_Props,
                               Scalar_Style => Plain,
                               Content => Text.Empty);
            P.Levels.Pop;
            return True;
         when Lexer.Folded_Scalar | Lexer.Literal_Scalar =>
            E := Event'(
              Start_Position => P.Current.Start_Pos,
              End_Position   => P.Current.End_Pos,
              Kind => Scalar,
              Scalar_Properties => P.Inline_Props,
              Scalar_Style => (if P.Current.Kind = Lexer.Folded_Scalar then
                                  Folded else Literal),
              Content => Lexer.Current_Content (P.L));
            P.Levels.Pop;
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when others =>
            raise Parser_Error with "Illegal content at '---' line: " &
              P.Current.Kind'Img;
      end case;
   end After_Directives_End;

   function Before_Implicit_Root (P : in out Class;
                                  E : out Event) return Boolean is
      pragma Unreferenced (E);
   begin
      if P.Current.Kind /= Lexer.Indentation then
         raise Parser_Error with "Unexpected token (expected line start) :" &
           P.Current.Kind'Img;
      end if;
      P.Inline_Start := P.Current.End_Pos;
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      P.Current := Lexer.Next_Token (P.L);
      case P.Current.Kind is
         when Lexer.Seq_Item_Ind | Lexer.Map_Key_Ind | Lexer.Map_Value_Ind =>
            P.Levels.Top.State := After_Compact_Parent'Access;
            return False;
         when Lexer.Scalar_Token_Kind =>
            P.Levels.Top.State := Require_Implicit_Map_Start'Access;
            return False;
         when Lexer.Node_Property_Kind =>
            P.Levels.Top.State := Require_Implicit_Map_Start'Access;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
            return False;
         when Lexer.Flow_Map_Start | Lexer.Flow_Seq_Start =>
            P.Levels.Top.State := After_Compact_Parent_Props'Access;
            return False;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected collection start): " &
              P.Current.Kind'Img;
      end case;
   end Before_Implicit_Root;

   function Require_Implicit_Map_Start (P : in out Class;
                                        E : out Event) return Boolean is
      Header_End : Mark;
   begin
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      case P.Current.Kind is
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
            Header_End := P.Current.Start_Pos;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Map_Value_Ind then
               P.Cached := E;
               E := Event'(Start_Position => P.Header_Start,
                                  End_Position   => Header_End,
                                  Kind => Mapping_Start,
                                  Collection_Properties => P.Header_Props,
                                  Collection_Style => Block);
               P.Header_Props := Default_Properties;
               P.Levels.Top.State := After_Implicit_Map_Start'Access;
            else
               if not Is_Empty (P.Header_Props) then
                  raise Parser_Error with "Alias may not have properties2";
               end if;
               --  alias is allowed on document root without '---'
               P.Levels.Pop;
            end if;
            return True;
         when Lexer.Flow_Scalar_Token_Kind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Inline_Props := Default_Properties;
            Header_End := P.Current.Start_Pos;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Map_Value_Ind then
               if Lexer.Last_Scalar_Was_Multiline (P.L) then
                  raise Parser_Error with
                    "Implicit mapping key may not be multiline";
               end if;
               P.Cached := E;
               E := Event'(Start_Position        => P.Header_Start,
                           End_Position          => Header_End,
                           Kind                  => Mapping_Start,
                           Collection_Properties => P.Header_Props,
                           Collection_Style      => Block);
               P.Header_Props := Default_Properties;
               P.Levels.Top.State := After_Implicit_Map_Start'Access;
            elsif P.Current.Kind in Lexer.Indentation | Lexer.Document_End |
              Lexer.Directives_End | Lexer.Stream_End then
               raise Parser_Error with "Scalar at root level requires '---'.";
            end if;
            return True;
         when Lexer.Flow_Map_Start | Lexer.Flow_Seq_Start =>
            P.Levels.Top.State := Before_Flow_Item_Props'Access;
            return False;
         when Lexer.Indentation =>
              raise Parser_Error with
                "Stand-alone node properties not allowed on non-header line";
         when others =>
            raise Parser_Error with
              "Unexpected token (expected implicit mapping key): " &
              P.Current.Kind'Img;
      end case;
   end Require_Implicit_Map_Start;

   function At_Block_Indentation (P : in out Class;
                                  E : out Event) return Boolean is
      Header_End : Mark;
   begin
      if P.Block_Indentation = P.Levels.Top.Indentation and then
         (P.Current.Kind /= Lexer.Seq_Item_Ind or else
          P.Levels.Element (P.Levels.Length - 2).State = In_Block_Seq'Access)
      then
         -- empty element is empty scalar
         E := Event'(Start_Position => P.Header_Start,
                            End_Position   => P.Header_Start,
                            Kind => Scalar,
                            Scalar_Properties => P.Header_Props,
                            Scalar_Style => Plain,
                            Content => Text.Empty);
         P.Header_Props := Default_Properties;
         P.Levels.Pop;
         P.Levels.Pop;
         return True;
      end if;
      P.Inline_Start := P.Current.Start_Pos;
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            if Is_Empty (P.Header_Props) then
               P.Levels.Top.State := Require_Inline_Block_Item'Access;
            else
               P.Levels.Top.State := Require_Implicit_Map_Start'Access;
            end if;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
            return False;
         when Lexer.Seq_Item_Ind =>
            E := Event'(Start_Position        => P.Header_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Sequence_Start,
                        Collection_Properties => P.Header_Props,
                        Collection_Style      => Block);
            P.Header_Props := Default_Properties;
            P.Levels.Top.all :=
              (In_Block_Seq'Access, Lexer.Recent_Indentation (P.L));
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push ((State => After_Compact_Parent'Access,
                            Indentation => Lexer.Recent_Indentation (P.L)));
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when Lexer.Map_Key_Ind =>
            E := Event'(Start_Position        => P.Header_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => P.Header_Props,
                        Collection_Style => Block);
            P.Header_Props := Default_Properties;
            P.Levels.Top.all :=
              (Before_Block_Map_Value'Access, Lexer.Recent_Indentation (P.L));
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push ((State => After_Compact_Parent'Access,
                            Indentation => Lexer.Recent_Indentation (P.L)));
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when Lexer.Flow_Scalar_Token_Kind =>
            P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Header_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Header_Props := Default_Properties;
            Header_End := P.Current.Start_Pos;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Map_Value_Ind then
               if Lexer.Last_Scalar_Was_Multiline (P.L) then
                  raise Parser_Error with
                    "Implicit mapping key may not be multiline";
               end if;
               P.Cached := E;
               E := Event'(Start_Position        => P.Header_Start,
                           End_Position          => Header_End,
                           Kind                  => Mapping_Start,
                           Collection_Properties => P.Cached.Scalar_Properties,
                           Collection_Style      => Block);
               P.Cached.Scalar_Properties := Default_Properties;
               P.Levels.Top.State := After_Implicit_Map_Start'Access;
            else
               P.Levels.Pop;
            end if;
            return True;
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
            P.Inline_Props := Default_Properties;
            Header_End := P.Current.Start_Pos;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Map_Value_Ind then
               P.Cached := E;
               E := Event'(Start_Position        => P.Header_Start,
                           End_Position          => Header_End,
                           Kind                  => Mapping_Start,
                           Collection_Properties => P.Header_Props,
                           Collection_Style      => Block);
               P.Header_Props := Default_Properties;
               P.Levels.Top.State := After_Implicit_Map_Start'Access;
            elsif not Is_Empty (P.Header_Props) then
               raise Parser_Error with "Alias may not have properties1";
            else
               P.Levels.Pop;
            end if;
            return True;
         when others =>
            P.Levels.Top.State := At_Block_Indentation_Props'Access;
            return False;
      end case;
   end At_Block_Indentation;

   function At_Block_Indentation_Props (P : in out Class;
                                        E : out Event) return Boolean is
      Header_End : Mark;
   begin
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      case P.Current.Kind is
         when Lexer.Map_Value_Ind =>
            P.Cached := Event'(Start_Position    => P.Inline_Start,
                               End_Position      => P.Current.End_Pos,
                               Kind              => Scalar,
                               Scalar_Properties => P.Inline_Props,
                               Scalar_Style      => Plain,
                               Content           => Text.Empty);
            P.Inline_Props := Default_Properties;
            E := Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Mapping_Start,
                               Collection_Properties => P.Header_Props,
                               Collection_Style => Block);
            P.Header_Props := Default_Properties;
            P.Levels.Top.State := After_Implicit_Map_Start'Access;
            return True;
         when Lexer.Flow_Scalar_Token_Kind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Inline_Props := Default_Properties;
            Header_End := P.Current.Start_Pos;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Map_Value_Ind then
               if Lexer.Last_Scalar_Was_Multiline (P.L) then
                  raise Parser_Error with
                    "Implicit mapping key may not be multiline";
               end if;
               P.Cached := E;
               E := Event'(Start_Position        => P.Header_Start,
                           End_Position          => Header_End,
                           Kind                  => Mapping_Start,
                           Collection_Properties => P.Header_Props,
                           Collection_Style      => Block);
               P.Header_Props := Default_Properties;
               P.Levels.Top.State := After_Implicit_Map_Start'Access;
            else
               P.Levels.Pop;
            end if;
            return True;
         when Lexer.Flow_Map_Start =>
            E := Event'(Start_Position        => P.Header_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => P.Header_Props,
                        Collection_Style      => Flow);
            P.Header_Props := Default_Properties;
            P.Levels.Top.State := After_Flow_Map_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when Lexer.Flow_Seq_Start =>
            E := Event'(Start_Position        => P.Header_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Sequence_Start,
                        Collection_Properties => P.Header_Props,
                        Collection_Style      => Flow);
            P.Header_Props := Default_Properties;
            P.Levels.Top.State := After_Flow_Seq_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected block content): " &
              P.Current.Kind'Img;
      end case;
   end At_Block_Indentation_Props;

   function Before_Node_Properties (P : in out Class;
                                    E : out Event) return Boolean is
      pragma Unreferenced (E);
   begin
      case P.Current.Kind is
         when Lexer.Tag_Handle =>
            if P.Inline_Props.Tag /= Tags.Question_Mark then
               raise Parser_Error with "Only one tag allowed per element";
            end if;
            P.Inline_Props.Tag := Parse_Tag (P);
         when Lexer.Verbatim_Tag =>
            if P.Inline_Props.Tag /= Tags.Question_Mark then
               raise Parser_Error with "Only one tag allowed per element";
            end if;
            P.Inline_Props.Tag := Lexer.Current_Content (P.L);
         when Lexer.Anchor =>
            if P.Inline_Props.Anchor /= Text.Empty then
               raise Parser_Error with "Only one anchor allowed per element";
            end if;
            P.Inline_Props.Anchor :=
              P.Pool.From_String (Lexer.Short_Lexeme (P.L));
         when Lexer.Annotation_Handle =>
            declare
               NS : constant String := Lexer.Full_Lexeme (P.L);
            begin
               P.Current := Lexer.Next_Token (P.L);
               if P.Current.Kind /= Lexer.Suffix then
                  raise Parser_Error with
                    "Unexpected token (expected annotation suffix): " &
                    P.Current.Kind'Img;
               end if;
               if NS = "@@" then
                  E := Event'(Start_Position => P.Inline_Start,
                              End_Position => P.Current.Start_Pos,
                              Kind => Annotation_Start,
                              Annotation_Properties => P.Inline_Props,
                              Namespace => Standard_Annotation_Namespace,
                              Name => Lexer.Current_Content (P.L));
               else
                  E := Event'(Start_Position => P.Inline_Start,
                              End_Position => P.Current.Start_Pos,
                              Kind => Annotation_Start,
                              Annotation_Properties => P.Inline_Props,
                              Namespace => P.Pool.From_String (NS),
                              Name => Lexer.Current_Content (P.L));
               end if;
            end;
            P.Inline_Props := Default_Properties;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Params_Start then
               P.Current := Lexer.Next_Token (P.L);
               P.Levels.Push ((State => After_Param_Sep'Access,
                               Indentation => P.Block_Indentation));
            else
               P.Levels.Top.State := After_Annotation'Access;
            end if;
            return True;
         when Lexer.Indentation =>
            P.Header_Props := P.Inline_Props;
            P.Inline_Props := Default_Properties;
            P.Levels.Pop;
            return False;
         when Lexer.Alias =>
            raise Parser_Error with "Alias may not have node properties";
         when others =>
            P.Levels.Pop;
            return False;
      end case;
      P.Current := Lexer.Next_Token (P.L);
      return False;
   end Before_Node_Properties;

   function After_Compact_Parent (P : in out Class;
                                  E : out Event) return Boolean is
   begin
      P.Inline_Start := P.Current.Start_Pos;
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Top.State := After_Compact_Parent_Props'Access;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
         when Lexer.Seq_Item_Ind =>
            E := Event'(Start_Position        => P.Header_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Sequence_Start,
                        Collection_Properties => P.Header_Props,
                        Collection_Style      => Block);
            P.Header_Props := Default_Properties;
            P.Levels.Top.all := (In_Block_Seq'Access, Lexer.Recent_Indentation (P.L));
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push ((State => After_Compact_Parent'Access,
                            Indentation => <>));
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when Lexer.Map_Key_Ind =>
            E := Event'(Start_Position        => P.Header_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => P.Header_Props,
                        Collection_Style      => Block);
            P.Header_Props := Default_Properties;
            P.Levels.Top.all := (Before_Block_Map_Value'Access, Lexer.Recent_Indentation (P.L));
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push ((State => After_Compact_Parent'Access,
                            Indentation => <>));
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when others =>
            P.Levels.Top.State := After_Compact_Parent_Props'Access;
            return False;
      end case;
      return False;
   end After_Compact_Parent;

   function After_Compact_Parent_Props (P : in out Class;
                                      E : out Event) return Boolean is
   begin
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
            return False;
         when Lexer.Indentation =>
            P.Header_Start := P.Inline_Start;
            P.Levels.Top.all :=
              (State => At_Block_Indentation'Access,
               Indentation => P.Levels.Element (P.Levels.Length - 2).Indentation);
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            return False;
         when Lexer.Stream_End | Lexer.Document_End | Lexer.Directives_End =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.Start_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Inline_Props := Default_Properties;
            P.Levels.Pop;
            return True;
         when Lexer.Map_Value_Ind =>
            P.Cached := Event'(Start_Position    => P.Inline_Start,
                               End_Position      => P.Current.End_Pos,
                               Kind              => Scalar,
                               Scalar_Properties => P.Inline_Props,
                               Scalar_Style      => Plain,
                               Content           => Text.Empty);
            P.Inline_Props := Default_Properties;
            E := Event'(Start_Position        => P.Current.Start_Pos,
                        End_Position          => P.Current.Start_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => Default_Properties,
                        Collection_Style      => Block);
            P.Levels.Top.State := After_Implicit_Map_Start'Access;
            return True;
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
            declare
               Header_End : constant Mark := P.Current.Start_Pos;
            begin
               P.Current := Lexer.Next_Token (P.L);
               if P.Current.Kind = Lexer.Map_Value_Ind then
                  P.Cached := E;
                  E := Event'(Start_Position        => Header_End,
                              End_Position          => Header_End,
                              Kind                  => Mapping_Start,
                              Collection_Properties => Default_Properties,
                              Collection_Style      => Block);
                  P.Levels.Top.State := After_Implicit_Map_Start'Access;
               else
                  P.Levels.Pop;
               end if;
            end;
            return True;
         when Lexer.Scalar_Token_Kind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Inline_Props := Default_Properties;
            declare
               Header_End : constant Mark := P.Current.Start_Pos;
            begin
               P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
               P.Current := Lexer.Next_Token (P.L);
               if P.Current.Kind = Lexer.Map_Value_Ind then
                  if Lexer.Last_Scalar_Was_Multiline (P.L) then
                     raise Parser_Error with
                       "Implicit mapping key may not be multiline";
                  end if;
                  P.Cached := E;
                  E := Event'(Start_Position        => Header_End,
                              End_Position          => Header_End,
                              Kind                  => Mapping_Start,
                              Collection_Properties => Default_Properties,
                              Collection_Style      => Block);
                  P.Levels.Top.State := After_Implicit_Map_Start'Access;
               else
                  P.Levels.Pop;
               end if;
            end;
            return True;
         when Lexer.Flow_Map_Start =>
            E := Event'(Start_Position        => P.Inline_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => P.Inline_Props,
                        Collection_Style      => Flow);
            P.Inline_Props := Default_Properties;
            P.Levels.Top.State := After_Flow_Map_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when Lexer.Flow_Seq_Start =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind => Sequence_Start,
                        Collection_Properties => P.Inline_Props,
                        Collection_Style => Flow);
            P.Inline_Props := Default_Properties;
            P.Levels.Top.State := After_Flow_Seq_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected newline or flow item start): " &
              P.Current.Kind'Img;
      end case;
   end After_Compact_Parent_Props;

   function After_Block_Parent (P : in out Class;
                                E : out Event) return Boolean is
      pragma Unreferenced (E);
   begin
      P.Inline_Start := P.Current.Start_Pos;
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Top.State := After_Block_Parent_Props'Access;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
         when Lexer.Seq_Item_Ind | Lexer.Map_Key_Ind =>
            raise Parser_Error with
              "Compact notation not allowed after implicit key";
         when others =>
            P.Levels.Top.State := After_Block_Parent_Props'Access;
      end case;
      return False;
   end After_Block_Parent;

   function After_Block_Parent_Props (P : in out Class;
                                      E : out Event) return Boolean is
   begin
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
            return False;
         when Lexer.Map_Value_Ind =>
            raise Parser_Error with
              "Compact notation not allowed after implicit key";
         when Lexer.Scalar_Token_Kind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Inline_Props := Default_Properties;
            P.Current := Lexer.Next_Token (P.L);
            if P.Current.Kind = Lexer.Map_Value_Ind then
               raise Parser_Error with
                 "Compact notation not allowed after implicit key";
            end if;
            P.Levels.Pop;
            return True;
         when others =>
            P.Levels.Top.State := After_Compact_Parent_Props'Access;
            return False;
      end case;
   end After_Block_Parent_Props;

   function Require_Inline_Block_Item (P : in out Class;
                                       E : out Event) return Boolean is
      pragma Unreferenced (E);
   begin
      P.Levels.Top.Indentation := Lexer.Recent_Indentation (P.L);
      case P.Current.Kind is
         when Lexer.Indentation =>
            raise Parser_Error with
              "Node properties may not stand alone on a line";
         when others =>
            P.Levels.Top.State := After_Compact_Parent_Props'Access;
            return False;
      end case;
   end Require_Inline_Block_Item;

   function Before_Doc_End (P : in out Class;
                               E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Document_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Document_End,
                        Implicit_End   => False);
            P.Levels.Top.State := Before_Doc'Access;
            Reset_Tag_Handles (P);
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Stream_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Document_End,
                        Implicit_End   => True);
            P.Levels.Pop;
         when Lexer.Directives_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Document_End,
                        Implicit_End   => True);
            Reset_Tag_Handles (P);
            P.Levels.Top.State := Before_Doc'Access;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected document end): " & P.Current.Kind'Img;
      end case;
      return True;
   end Before_Doc_End;

   function In_Block_Seq (P : in out Class;
                          E : out Event) return Boolean is
   begin
      if P.Block_Indentation > P.Levels.Top.Indentation then
         raise Parser_Error with "Invalid indentation (bseq); got" &
           P.Block_Indentation'Img & ", expected" & P.Levels.Top.Indentation'Img;
      end if;
      case P.Current.Kind is
         when Lexer.Seq_Item_Ind =>
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push ((State => After_Compact_Parent'Access,
                            Indentation => P.Block_Indentation));
            return False;
         when others =>
            if P.Levels.Element (P.Levels.Length - 2).Indentation =
              P.Levels.Top.Indentation then
               E := Event'(Start_Position => P.Current.Start_Pos,
                           End_Position   => P.Current.End_Pos,
                           Kind           => Sequence_End);
               P.Levels.Pop;
               P.Levels.Pop;
               return True;
            else
               raise Parser_Error with
                 "Illegal token (expected block sequence indicator): " &
                 P.Current.Kind'Img;
            end if;
      end case;
   end In_Block_Seq;

   function After_Implicit_Map_Start (P : in out Class;
                                      E : out Event) return Boolean is
   begin
      E := P.Cached;
      P.Levels.Top.State := After_Implicit_Key'Access;
      return True;
   end After_Implicit_Map_Start;

   function Before_Block_Map_Key (P : in out Class;
                                  E : out Event) return Boolean is
   begin
      if P.Block_Indentation > P.Levels.Top.Indentation then
         raise Parser_Error with "Invalid indentation (bmk); got" &
           P.Block_Indentation'Img & ", expected" & P.Levels.Top.Indentation'Img &
         ", token = " & P.Current.Kind'Img;
      end if;
      case P.Current.Kind is
         when Lexer.Map_Key_Ind =>
            P.Levels.Top.State := Before_Block_Map_Value'Access;
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push ((State => After_Compact_Parent'Access,
                            Indentation => P.Levels.Top.Indentation));
            P.Current := Lexer.Next_Token (P.L);
            return False;
         when Lexer.Node_Property_Kind =>
            P.Levels.Top.State := At_Block_Map_Key_Props'Access;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
            return False;
         when Lexer.Flow_Scalar_Token_Kind =>
            P.Levels.Top.State := At_Block_Map_Key_Props'Access;
            return False;
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Top.State := After_Implicit_Key'Access;
            return True;
         when Lexer.Map_Value_Ind =>
            E := Event'(Start_Position    => P.Current.Start_Pos,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => Default_Properties,
                        Scalar_Style      => Plain,
                               Content => Text.Empty);
            P.Levels.Top.State := Before_Block_Map_Value'Access;
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected mapping key): " &
              P.Current.Kind'Img;
      end case;
   end Before_Block_Map_Key;

   function At_Block_Map_Key_Props (P : in out Class;
                                    E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
         when Lexer.Flow_Scalar_Token_Kind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Inline_Props := Default_Properties;
            if Lexer.Last_Scalar_Was_Multiline (P.L) then
               raise Parser_Error with
                 "Implicit mapping key may not be multiline";
            end if;
         when Lexer.Map_Value_Ind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.Start_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Inline_Props := Default_Properties;
            P.Levels.Top.State := After_Implicit_Key'Access;
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected implicit mapping key): " &
              P.Current.Kind'Img;
      end case;
      P.Current := Lexer.Next_Token (P.L);
      P.Levels.Top.State := After_Implicit_Key'Access;
      return True;
   end At_Block_Map_Key_Props;

   function After_Implicit_Key (P : in out Class;
                                E : out Event) return Boolean is
      pragma Unreferenced (E);
   begin
      if P.Current.Kind /= Lexer.Map_Value_Ind then
         raise Parser_Error with "Unexpected token (expected ':'): " &
           P.Current.Kind'Img;
      end if;
      P.Current := Lexer.Next_Token (P.L);
      P.Levels.Top.State := Before_Block_Map_Key'Access;
      P.Levels.Push ((State => Before_Block_Indentation'Access,
                      Indentation => <>));
      P.Levels.Push
        ((State => After_Block_Parent'Access,
          Indentation => P.Levels.Top.Indentation));
      return False;
   end After_Implicit_Key;

   function Before_Block_Map_Value (P : in out Class;
                                    E : out Event) return Boolean is
   begin
      if P.Block_Indentation > P.Levels.Top.Indentation then
         raise Parser_Error with "Invalid indentation (bmv)";
      end if;
      case P.Current.Kind is
         when Lexer.Map_Value_Ind =>
            P.Levels.Top.State := Before_Block_Map_Key'Access;
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            P.Levels.Push
              ((State => After_Compact_Parent'Access,
                Indentation => P.Levels.Top.Indentation));
            P.Current := Lexer.Next_Token (P.L);
            return False;
         when Lexer.Map_Key_Ind | Lexer.Flow_Scalar_Token_Kind |
            Lexer.Node_Property_Kind =>
            --  the value is allowed to be missing after an explicit key
            E := Event'(Start_Position    => P.Current.Start_Pos,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => Default_Properties,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Levels.Top.State := Before_Block_Map_Key'Access;
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected mapping value): " &
              P.Current.Kind'Img;
      end case;
   end Before_Block_Map_Value;

   function Before_Block_Indentation (P : in out Class;
                                      E : out Event) return Boolean is
      procedure End_Block_Node is
      begin
         if P.Levels.Top.State = Before_Block_Map_Key'Access then
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Mapping_End);
         elsif P.Levels.Top.State = Before_Block_Map_Value'Access then
            E := Event'(Start_Position    => P.Current.Start_Pos,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => Default_Properties,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Levels.Top.State := Before_Block_Map_Key'Access;
            P.Levels.Push ((State => Before_Block_Indentation'Access,
                            Indentation => <>));
            return;
         elsif P.Levels.Top.State = In_Block_Seq'Access then
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Sequence_End);
         elsif P.Levels.Top.State = At_Block_Indentation'Access then
            E := Event'(Start_Position => P.Header_Start,
                        End_Position   => P.Header_Start,
                        Kind => Scalar,
                        Scalar_Properties => P.Header_Props,
                        Scalar_Style => Plain,
                        Content => Text.Empty);
            P.Header_Props := Default_Properties;
         elsif P.Levels.Top.State = Before_Block_Indentation'Access then
            raise Parser_Error with "Unexpected double Before_Block_Indentation";
         else
            raise Parser_Error with "Internal error (please report this bug)";
         end if;
         P.Levels.Pop;
      end End_Block_Node;
   begin
      P.Levels.Pop;
      case P.Current.Kind is
         when Lexer.Indentation =>
            P.Block_Indentation := Lexer.Current_Indentation (P.L);
            if P.Block_Indentation < P.Levels.Top.Indentation then
               End_Block_Node;
               return True;
            else
               P.Current := Lexer.Next_Token (P.L);
               return False;
            end if;
         when Lexer.Stream_End | Lexer.Document_End | Lexer.Directives_End =>
            P.Block_Indentation := 0;
            if P.Levels.Top.State /= Before_Doc_End'Access then
               End_Block_Node;
               return True;
            else
               return False;
            end if;
         when others =>
            raise Parser_Error with
              "Unexpected content after node in block context (expected newline): "
              & P.Current.Kind'Img;
      end case;
   end Before_Block_Indentation;

   function Before_Flow_Item (P : in out Class;
                              E : out Event) return Boolean is
   begin
      P.Inline_Start := P.Current.Start_Pos;
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Top.State := Before_Flow_Item_Props'Access;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when others =>
            P.Levels.Top.State := Before_Flow_Item_Props'Access;
      end case;
      return False;
   end Before_Flow_Item;

   function Before_Flow_Item_Props (P : in out Class;
                                    E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Node_Property_Kind =>
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
         when Lexer.Alias =>
            E := Event'(Start_Position => P.Inline_Start,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Alias,
                        Target         => P.Pool.From_String (Lexer.Short_Lexeme (P.L)));
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
         when Lexer.Scalar_Token_Kind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => To_Style (P.Current.Kind),
                        Content           => Lexer.Current_Content (P.L));
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
         when Lexer.Flow_Map_Start =>
            E := Event'(Start_Position        => P.Inline_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => P.Inline_Props,
                        Collection_Style      => Flow);
            P.Levels.Top.State := After_Flow_Map_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Flow_Seq_Start =>
            E := Event'(Start_Position        => P.Inline_Start,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Sequence_Start,
                        Collection_Properties => P.Inline_Props,
                        Collection_Style      => Flow);
            P.Levels.Top.State := After_Flow_Seq_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Flow_Map_End | Lexer.Flow_Seq_End |
              Lexer.Flow_Separator | Lexer.Map_Value_Ind =>
            E := Event'(Start_Position    => P.Inline_Start,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => P.Inline_Props,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Levels.Pop;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected flow node): " & P.Current.Kind'Img;
      end case;
      P.Inline_Props := Default_Properties;
      return True;
   end Before_Flow_Item_Props;

   function After_Flow_Map_Key (P : in out Class;
                                E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Map_Value_Ind =>
            P.Levels.Top.State := After_Flow_Map_Value'Access;
            P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
            P.Current := Lexer.Next_Token (P.L);
            return False;
         when Lexer.Flow_Separator | Lexer.Flow_Map_End =>
            E := Event'(Start_Position    => P.Current.Start_Pos,
                        End_Position      => P.Current.End_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => Default_Properties,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Levels.Top.State := After_Flow_Map_Value'Access;
            return True;
         when others =>
            raise Parser_Error with "Unexpected token (expected ':'): " &
              P.Current.Kind'Img;
      end case;
   end After_Flow_Map_Key;

   function After_Flow_Map_Value (P : in out Class;
                                  E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Flow_Separator =>
            P.Levels.Top.State := After_Flow_Map_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return False;
         when Lexer.Flow_Map_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Mapping_End);
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when Lexer.Flow_Scalar_Token_Kind | Lexer.Map_Key_Ind |
              Lexer.Anchor | Lexer.Alias | Lexer.Annotation_Handle |
              Lexer.Flow_Map_Start | Lexer.Flow_Seq_Start =>
            raise Parser_Error with "Missing ','";
         when others =>
            raise Parser_Error with "Unexpected token (expected ',' or '}'): " &
              P.Current.Kind'Img;
      end case;
   end After_Flow_Map_Value;

   function After_Flow_Seq_Item (P : in out Class;
                                 E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Flow_Separator =>
            P.Levels.Top.State := After_Flow_Seq_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return False;
         when Lexer.Flow_Seq_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Sequence_End);
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when Lexer.Flow_Scalar_Token_Kind | Lexer.Map_Key_Ind |
              Lexer.Anchor | Lexer.Alias | Lexer.Annotation_Handle |
              Lexer.Flow_Map_Start | Lexer.Flow_Seq_Start =>
            raise Parser_Error with "Missing ','";
         when others =>
            raise Parser_Error with "Unexpected token (expected ',' or ']'): " &
              P.Current.Kind'Img;
      end case;
   end After_Flow_Seq_Item;

   function After_Flow_Map_Sep (P : in out Class;
                                E : out Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Map_Key_Ind =>
            P.Current := Lexer.Next_Token (P.L);
         when Lexer.Flow_Map_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind           => Mapping_End);
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when others => null;
      end case;
      P.Levels.Top.State := After_Flow_Map_Key'Access;
      P.Levels.Push ((State => Before_Flow_Item'Access, Indentation => <>));
      return False;
   end After_Flow_Map_Sep;

   function Possible_Next_Sequence_Item (P : in out Class;
                                         E : out Event;
                                         End_Token : Lexer.Token_Kind;
                                         After_Props, After_Item : State_Type)
                                         return Boolean is
   begin
      P.Inline_Start := P.Current.Start_Pos;
      case P.Current.Kind is
         when Lexer.Flow_Separator =>
            E := Event'(Start_Position    => P.Current.Start_Pos,
                        End_Position      => P.Current.Start_Pos,
                        Kind              => Scalar,
                        Scalar_Properties => Default_Properties,
                        Scalar_Style      => Plain,
                        Content           => Text.Empty);
            P.Current := Lexer.Next_Token (P.L);
            return True;
         when Lexer.Node_Property_Kind =>
            P.Levels.Top.State := After_Props;
            P.Levels.Push ((State => Before_Node_Properties'Access,
                            Indentation => <>));
            return False;
         when Lexer.Flow_Scalar_Token_Kind =>
            P.Levels.Top.State := After_Props;
            return False;
         when Lexer.Map_Key_Ind =>
            P.Levels.Top.State := After_Item;
            E := Event'(Start_Position        => P.Current.Start_Pos,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => Default_Properties,
                        Collection_Style      => Flow);
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Push ((State => Before_Pair_Value'Access, others => <>));
            P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
            return True;
         when Lexer.Map_Value_Ind =>
            P.Levels.Top.State := After_Item;
            E := Event'(Start_Position        => P.Current.Start_Pos,
                        End_Position          => P.Current.End_Pos,
                        Kind                  => Mapping_Start,
                        Collection_Properties => Default_Properties,
                        Collection_Style      => Flow);
            P.Levels.Push ((State => At_Empty_Pair_Key'Access, others => <>));
            return True;
         when others =>
            if P.Current.Kind = End_Token then
               E := Event'(Start_Position => P.Current.Start_Pos,
                           End_Position   => P.Current.End_Pos,
                           Kind           => Sequence_End);
               P.Current := Lexer.Next_Token (P.L);
               P.Levels.Pop;
               return True;
            else
               P.Levels.Top.State := After_Item;
               P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
               return False;
            end if;
      end case;
   end Possible_Next_Sequence_Item;


   function After_Flow_Seq_Sep (P : in out Class;
                                E : out Event) return Boolean is
   begin
      return Possible_Next_Sequence_Item (P, E, Lexer.Flow_Seq_End,
                                          After_Flow_Seq_Sep_Props'Access,
                                          After_Flow_Seq_Item'Access);
   end After_Flow_Seq_Sep;

   function Forced_Next_Sequence_Item (P : in out Class;
                                       E : out Event) return Boolean is
   begin
      if P.Current.Kind in Lexer.Flow_Scalar_Token_Kind then
         E := Event'(Start_Position => P.Inline_Start,
                     End_Position   => P.Current.End_Pos,
                     Kind => Scalar,
                     Scalar_Properties => P.Inline_Props,
                     Scalar_Style => To_Style (P.Current.Kind),
                     Content => Lexer.Current_Content (P.L));
         P.Inline_Props := Default_Properties;
         P.Current := Lexer.Next_Token (P.L);
         if P.Current.Kind = Lexer.Map_Value_Ind then
            P.Cached := E;
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.Start_Pos,
                        Kind => Mapping_Start,
                        Collection_Properties => Default_Properties,
                        Collection_Style => Flow);

            P.Levels.Push ((State => After_Implicit_Pair_Start'Access,
                            Indentation => <>));
         end if;
         return True;
      else
         P.Levels.Push ((State => Before_Flow_Item_Props'Access, others => <>));
         return False;
      end if;
   end Forced_Next_Sequence_Item;

   function After_Flow_Seq_Sep_Props (P : in out Class;
                                      E : out Event) return Boolean is
   begin
      P.Levels.Top.State := After_Flow_Seq_Item'Access;
      return Forced_Next_Sequence_Item (P, E);
   end After_Flow_Seq_Sep_Props;

   function At_Empty_Pair_Key (P : in out Class; E : out Event) return Boolean
   is begin
      P.Levels.Top.State := Before_Pair_Value'Access;
      E := Event'(Start_Position => P.Current.Start_Pos,
                  End_Position   => P.Current.Start_Pos,
                  Kind           => Scalar,
                  Scalar_Properties => Default_Properties,
                  Scalar_Style   => Plain,
                  Content        => Text.Empty);
      return True;
   end At_Empty_Pair_Key;

   function Before_Pair_Value (P : in out Class;
                               E : out Event) return Boolean is
   begin
      if P.Current.Kind = Lexer.Map_Value_Ind then
         P.Levels.Top.State := After_Pair_Value'Access;
         P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
         P.Current := Lexer.Next_Token (P.L);
         return False;
      else
         --  pair ends here without value.
         E := Event'(Start_Position    => P.Current.Start_Pos,
                     End_Position      => P.Current.End_Pos,
                     Kind              => Scalar,
                     Scalar_Properties => Default_Properties,
                     Scalar_Style      => Plain,
                     Content           => Text.Empty);
         P.Levels.Pop;
         return True;
      end if;
   end Before_Pair_Value;

   function After_Implicit_Pair_Start (P : in out Class;
                                       E : out Event) return Boolean is
   begin
      E := P.Cached;
      P.Current := Lexer.Next_Token (P.L);
      P.Levels.Top.State := After_Pair_Value'Access;
      P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
      return True;
   end After_Implicit_Pair_Start;

   function After_Pair_Value (P : in out Class;
                              E : out Event) return Boolean is
   begin
      E := Event'(Start_Position => P.Current.Start_Pos,
                  End_Position   => P.Current.End_Pos,
                  Kind           => Mapping_End);
      P.Levels.Pop;
      return True;
   end After_Pair_Value;

   function After_Param_Sep (P : in out Class; E : out Event)
                             return Boolean is
   begin
      return Possible_Next_Sequence_Item (P, E, Lexer.Params_End,
                                          After_Param_Sep_Props'Access,
                                          After_Param'Access);
   end After_Param_Sep;

   function After_Param_Sep_Props
     (P : in out Class; E : out Event) return Boolean is
   begin
      P.Levels.Top.State := After_Param'Access;
      return Forced_Next_Sequence_Item (P, E);
   end After_Param_Sep_Props;

   function After_Param (P : in out Class; E : out Event)
                         return Boolean is
   begin
      case P.Current.Kind is
         when Lexer.Flow_Separator =>
            P.Levels.Top.State := After_Param_Sep'Access;
            P.Current := Lexer.Next_Token (P.L);
            return False;
         when Lexer.Params_End =>
            E := Event'(Start_Position => P.Current.Start_Pos,
                        End_Position   => P.Current.End_Pos,
                        Kind => Annotation_End);
            P.Current := Lexer.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when Lexer.Flow_Scalar_Token_Kind | Lexer.Map_Key_Ind |
              Lexer.Anchor | Lexer.Alias | Lexer.Annotation_Handle |
              Lexer.Flow_Map_Start | Lexer.Flow_Seq_Start =>
            raise Parser_Error with "Missing ','";
         when others =>
            raise Parser_Error with "Unexpected token (expected ',' or ')'): " &
              P.Current.Kind'Img;
      end case;
   end After_Param;

   function After_Annotation (P : in out Class; E : out Event)
                              return Boolean is
   begin
      E := Event'(Start_Position => P.Current.Start_Pos,
                  End_Position => P.Current.Start_Pos,
                  Kind => Annotation_End);
      P.Levels.Pop;
      return True;
   end After_Annotation;

end Yaml.Parser;
