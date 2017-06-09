with Ada.Containers;

package body Yaml.Parsing is
   use type Lexing.Token_Kind;
   use Yaml.Strings;

   procedure Init (P : not null access Parser_Implementation) with Inline is
   begin
      P.Levels.Push ((State => At_Stream_Start'Access, Indentation => -2));
      Tag_Handle_Sets.Init (P.Tag_Handles, P.Pool, 16);
   end Init;

   procedure Parse (P : in out Parser; Input : Sources.Source_Access) is
      Pool : String_Pool;
   begin
      Create (Pool, 8092);
      declare
         PI : constant not null access Parser_Implementation :=
           new Parser_Implementation'(Streams.Stream_Implementation with
              L => <>, Pool => Pool, Levels => Level_Stacks.New_Stack (32),
              Current => <>, Cached => <>, Implicit_Key => False,
              Tag_Handles => <>);
      begin
         Lexing.Init (PI.L, Input, Pool);
         Init (PI);
         Streams.Create (P, Streams.Implementation_Access (PI));
      end;
   end Parse;

   procedure Parse (P : in out Parser; Input : String) is
      Pool : String_Pool;
   begin
      Create (Pool, 8092);
      declare
         PI : constant not null access Parser_Implementation :=
           new Parser_Implementation'(Streams.Stream_Implementation with
              L => <>, Pool => Pool, Levels => Level_Stacks.New_Stack (32),
              Current => <>, Cached => <>, Implicit_Key => False,
              Tag_Handles => <>);
      begin
         Lexing.Init (PI.L, Input, Pool);
         Init (PI);
         Streams.Create (P, Streams.Implementation_Access (PI));
      end;
   end Parse;

   procedure Fetch (Stream : in out Parser_Implementation;
                    E : out Events.Event) is
   begin
      while not Stream.Levels.Top.State (Stream, E) loop
         null;
      end loop;
   end Fetch;


   procedure Reset_Tag_Handles (P : in out Parser_Implementation'Class) is
   begin
      Tag_Handle_Sets.Clear (P.Tag_Handles);
      pragma Warnings (Off);
      if P.Tag_Handles.Set ("!", Strings.From_String (P.Pool, "!")) and
        P.Tag_Handles.Set ("!!",
                           Strings.From_String (P.Pool, "tag:yaml.org,2002:"))
      then
         null;
      end if;
      pragma Warnings (On);
   end Reset_Tag_Handles;

   function Parse_Tag (P : in out Parser_Implementation'Class) return Content is
      use type Ada.Containers.Hash_Type;
      Tag_Handle : constant String := Lexing.Full_Lexeme (P.L);
      Holder : constant access constant Tag_Handle_Sets.Holder :=
        P.Tag_Handles.Get (Tag_Handle, False);
   begin
      if Holder.Hash = 0 then
         raise Parser_Error with
           "Unknown tag handle: " & Tag_Handle;
      end if;
      P.Current := Lexing.Next_Token (P.L);
      if P.Current.Kind /= Lexing.Tag_Uri then
         raise Parser_Error with "Unexpected token (expected tag suffix): " &
           P.Current.Kind'Img;
      end if;
      return Strings.From_String
        (P.Pool, Value (Holder.Value) & Value (Lexing.Current_Content (P.L)));
   end Parse_Tag;

   function At_Stream_Start (P : in out Parser_Implementation'Class;
                             E : out Events.Event) return Boolean is
   begin
      P.Levels.Top.all := (State => At_Stream_End'Access, Indentation => -2);
      P.Levels.Push ((State => Before_Doc'Access, Indentation => -1));
      E := Events.Event'(Kind => Events.Stream_Start,
                         Start_Position => (Line => 1, Column => 1, Index => 1),
                         End_Position => (Line => 1, Column => 1, Index => 1));
      P.Current := Lexing.Next_Token (P.L);
      Reset_Tag_Handles (P);
      return True;
   end At_Stream_Start;

   function At_Stream_End (P : in out Parser_Implementation'Class;
                           E : out Events.Event) return Boolean is
      T : constant Lexing.Token := Lexing.Next_Token (P.L);
   begin
      E := Events.Event'(Kind => Events.Stream_End,
                         Start_Position => T.Start_Pos,
                         End_Position => T.End_Pos);
      return True;
   end At_Stream_End;

   function Before_Doc (P : in out Parser_Implementation'Class;
                         E : out Events.Event) return Boolean is
      Version : Content := Null_Content;
   begin
      loop
         case P.Current.Kind is
            when Lexing.Document_End =>
               Reset_Tag_Handles (P);
            when Lexing.Directives_End =>
               P.Current := Lexing.Next_Token (P.L);
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position => P.Current.End_Pos,
                                  Kind => Events.Document_Start,
                                  Implicit_Start => False,
                                  Version => Version);
               P.Levels.Top.State := Before_Doc_End'Access;
               P.Levels.Push ((State => Before_Block_Item'Access,
                                 Indentation => -1));
               return True;
            when Lexing.Stream_End =>
               P.Levels.Pop;
               return False;
            when Lexing.Indentation =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Document_Start,
                                  Implicit_Start => True,
                                  Version => Version);
               P.Levels.Top.State := Before_Doc_End'Access;
               P.Levels.Push ((State => Before_Block_Item'Access,
                                 Indentation => -1));
               return True;
            when Lexing.Yaml_Directive =>
               P.Current := Lexing.Next_Token (P.L);
               if P.Current.Kind /= Lexing.Directive_Param then
                  raise Parser_Error with
                    "Invalid token (expected YAML version string): " &
                    P.Current.Kind'Img;
               elsif Version /= Null_Content then
                  raise Parser_Error with
                    "Duplicate YAML directive";
               end if;
               Version := From_String (P.Pool, Lexing.Full_Lexeme (P.L));
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Tag_Directive =>
               P.Current := Lexing.Next_Token (P.L);
               if P.Current.Kind /= Lexing.Tag_Handle then
                  raise Parser_Error with
                    "Invalid token (expected tag handle): " & P.Current.Kind'Img;
               end if;
               declare
                  Tag_Handle : constant String := Lexing.Full_Lexeme (P.L);
                  Holder : access Tag_Handle_Sets.Holder;
               begin
                  P.Current := Lexing.Next_Token (P.L);
                  if P.Current.Kind /= Lexing.Tag_Uri then
                     raise Parser_Error with
                       "Invalid token (expected tag URI): " & P.Current.Kind'Img;
                  end if;
                  if Tag_Handle = "!" or Tag_Handle = "!!" then
                     Holder := Tag_Handle_Sets.Get (P.Tag_Handles, Tag_Handle, False);
                     Holder.Value := Lexing.Current_Content (P.L);
                  else
                     if not Tag_Handle_Sets.Set (P.Tag_Handles, Tag_Handle,
                                                 Lexing.Current_Content (P.L)) then
                        raise Parser_Error with
                          "Redefinition of tag handle " & Tag_Handle;
                     end if;
                  end if;
               end;
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Unknown_Directive =>
               raise Parser_Error with "Not implemented: unknown directives";
            when others =>
               raise Parser_Error with
                 "Unexpected token (expected directive or document start): " &
                 P.Current.Kind'Img;
         end case;
      end loop;
   end Before_Doc;

   function Before_Doc_End (P : in out Parser_Implementation'Class;
                               E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Document_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Document_End,
                               Implicit_End => False);
            P.Levels.Top.State := Before_Doc'Access;
            Reset_Tag_Handles (P);
            P.Current := Lexing.Next_Token (P.L);
         when Lexing.Stream_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Document_End,
                               Implicit_End => True);
            P.Levels.Pop;
         when Lexing.Directives_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Document_End,
                               Implicit_End => True);
            Reset_Tag_Handles (P);
            P.Levels.Top.State := Before_Doc'Access;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected document end): " & P.Current.Kind'Img;
      end case;
      return True;
   end Before_Doc_End;

   function To_Style (T : Lexing.Scalar_Token_Kind)
                      return Events.Scalar_Style_Type is
     (case T is
         when Lexing.Plain_Scalar => Events.Plain,
         when Lexing.Single_Quoted_Scalar => Events.Single_Quoted,
         when Lexing.Double_Quoted_Scalar => Events.Double_Quoted,
         when Lexing.Literal_Scalar => Events.Literal,
         when Lexing.Folded_Scalar => Events.Folded) with Inline;

   function Is_Empty (A : Events.Attributes) return Boolean is
     (A.Anchor = Null_Content and A.Tag = Null_Content and
      Events.Content_Stacks.Length (A.Annotations) = 0);

   function Before_Block_Item (P : in out Parser_Implementation'Class;
                               E : out Events.Event) return Boolean is
      Collection_Attrs : Events.Attributes;
      Attrs : Events.Attributes;
   begin
      loop
         case P.Current.Kind is
            when Lexing.Indentation =>
               if Lexing.Current_Indentation (P.L) <=
                 P.Levels.Top.Indentation then
                  if P.Implicit_Key then
                     raise Parser_Error with "Missing mapping value";
                  end if;
                  -- empty element is empty scalar
                  E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                     End_Position   => P.Current.End_Pos,
                                     Kind => Events.Scalar,
                                     Scalar_Attributes => Attrs,
                                     Scalar_Style => Events.Plain,
                                     Value => Null_Content);
                  P.Levels.Pop;
                  return True;
               elsif P.Implicit_Key then
                  raise Parser_Error with
                    "Implicit mapping key must occur on single line";
               end if;
               Collection_Attrs := Attrs;
               Attrs := (others => <>);
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Anchor =>
               if Attrs.Anchor /= Null_Content then
                  raise Parser_Error with "Only one anchor allowed per element";
               end if;
               Attrs.Anchor := From_String (P.Pool, Lexing.Short_Lexeme (P.L));
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Tag_Handle =>
               if Attrs.Tag /= Null_Content then
                  raise Parser_Error with "Only one tag allowed per element";
               end if;
               Attrs.Tag := Parse_Tag (P);
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Verbatim_Tag =>
               if Attrs.Tag /= Null_Content then
                  raise Parser_Error with "Only one tag allowed per element";
               end if;
               Attrs.Tag := Lexing.Current_Content (P.L);
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Annotation =>
               Attrs.Annotations.Push
                 (Strings.From_String (P.Pool, Lexing.Short_Lexeme (P.L)));
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Map_Key_Ind =>
               if not Is_Empty (Attrs) then
                  raise Parser_Error with
                    "properties not allowed in front of '?'";
               end if;
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.Start_Pos,
                                  Kind => Events.Mapping_Start,
                                  Collection_Attributes => Collection_Attrs,
                                  Collection_Style => Events.Block);
               P.Levels.Top.all := (State => Before_Block_Map_Value'Access,
                                Indentation => Lexing.Recent_Indentation (P.L));
               P.Levels.Push ((State => Before_Block_Item'Access,
                               Indentation => Lexing.Recent_Indentation (P.L)));
               P.Current := Lexing.Next_Token (P.L);
               return True;
            when Lexing.Scalar_Token_Kind | Lexing.Alias =>
               declare
                  Indent : constant Indentation_Type :=
                    Lexing.Recent_Indentation (P.L);
               begin
                  if P.Current.Kind = Lexing.Alias then
                     E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Alias,
                                  Target => From_String (P.Pool, Lexing.Short_Lexeme (P.L)));
                  else
                     E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                        End_Position   => P.Current.End_Pos,
                                        Kind => Events.Scalar,
                                        Scalar_Attributes => Attrs,
                                        Scalar_Style => To_Style (P.Current.Kind),
                                        Value => Lexing.Current_Content (P.L));
                  end if;
                  P.Current := Lexing.Next_Token (P.L);
                  if P.Implicit_Key then
                     return True;
                  end if;
                  P.Levels.Pop;
                  if P.Current.Kind = Lexing.Map_Value_Ind then
                     P.Cached := E;
                     E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                        End_Position   => P.Current.Start_Pos,
                                        Kind => Events.Mapping_Start,
                                        Collection_Attributes => Collection_Attrs,
                                        Collection_Style => Events.Block);
                     P.Levels.Push
                       ((State => After_Implicit_Map_Start'Access,
                         Indentation => Indent));
                  else
                     -- TODO: check collection attrs
                     null;
                  end if;
               end;
               return True;
            when Lexing.Seq_Item_Ind =>
               if not Is_Empty (Attrs) then
                  raise Parser_Error with
                    "properties not allowed in front of '-'";
               end if;
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Sequence_Start,
                                  Collection_Attributes => Collection_Attrs,
                                  Collection_Style => Events.Block);
               P.Levels.Top.all :=
                 (State => In_Block_Seq'Access,
                  Indentation => Lexing.Recent_Indentation (P.L));
               P.Levels.Push ((State => Before_Block_Item'Access,
                               Indentation => Lexing.Recent_Indentation (P.L)));
               P.Current := Lexing.Next_Token (P.L);
               return True;
            when Lexing.Flow_Map_Start =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Mapping_Start,
                                  Collection_Attributes => Attrs,
                                  Collection_Style => Events.Flow);
               P.Levels.Top.State := After_Flow_Map_Sep'Access;
               P.Current := Lexing.Next_Token (P.L);
               return True;
            when Lexing.Flow_Seq_Start =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Sequence_Start,
                                  Collection_Attributes => Attrs,
                                  Collection_Style => Events.Flow);
               P.Levels.Top.State := After_Flow_Seq_Sep'Access;
               P.Current := Lexing.Next_Token (P.L);
               return True;
            when others =>
               raise Parser_Error with "not implemented: " & P.Current.Kind'Img;
         end case;
      end loop;
   end Before_Block_Item;

   function In_Block_Seq (P : in out Parser_Implementation'Class;
                          E : out Events.Event) return Boolean is
      Indent : constant Lexing.Indentation_Type :=
        Lexing.Current_Indentation (P.L);
   begin
      case P.Current.Kind is
         when Lexing.Indentation =>
            if Indent < P.Levels.Top.Indentation then
                E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                   End_Position   => P.Current.End_Pos,
                                   Kind => Events.Sequence_End);
                P.Levels.Pop;
                return True;
            elsif Indent > P.Levels.Top.Indentation then
               raise Parser_Error with "Invalid indentation; got" &
                 Indent'Img & ", expected" & P.Levels.Top.Indentation'Img;
            end if;
         when Lexing.Document_End | Lexing.Directives_End | Lexing.Stream_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Sequence_End);
            P.Levels.Pop;
            return True;
         when others =>
            raise Parser_Error with "Illegal token (expected new line) " &
              P.Current.Kind'IMG;
      end case;
      P.Current := Lexing.Next_Token (P.L);
      if P.Current.Kind /= Lexing.Seq_Item_Ind then
         raise Parser_Error with
           "Illegal token (expected block sequence indicator): " &
           P.Current.Kind'Img;
      end if;
      P.Current := Lexing.Next_Token (P.L);
      P.Levels.Push
        ((State => Before_Block_Item'Access, Indentation => Indent));
      return False;
   end In_Block_Seq;

   function After_Implicit_Map_Start (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean is
   begin
      E := P.Cached;
      P.Levels.Top.State := After_Implicit_Key'Access;
      return True;
   end After_Implicit_Map_Start;

   function Before_Block_Map_Key (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean is
      Indent : constant Lexing.Indentation_Type :=
        Lexing.Current_Indentation (P.L);
   begin
      case P.Current.Kind is
         when Lexing.Indentation =>
            if Indent < P.Levels.Top.Indentation then
                E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                   End_Position   => P.Current.End_Pos,
                                   Kind => Events.Mapping_End);
                P.Levels.Pop;
                return True;
            elsif Indent > P.Levels.Top.Indentation then
               raise Parser_Error with "Invalid indentation";
            end if;
         when Lexing.Document_End | Lexing.Directives_End | Lexing.Stream_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Mapping_End);
            P.Levels.Pop;
            return True;
         when others =>
            raise Parser_Error with "Illegal token (expected new line) " &
              P.Current.Kind'IMG;
      end case;
      P.Current := Lexing.Next_Token (P.L);
      return At_Block_Map_Key (P, E);
   end Before_Block_Map_Key;

   function At_Block_Map_Key (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Map_Key_Ind =>
            P.Levels.Top.State := Before_Block_Map_Value'Access;
            P.Levels.Push
              ((State => Before_Block_Item'Access,
                Indentation => P.Levels.Top.Indentation));
            P.Current := Lexing.Next_Token (P.L);
            return False;
         when Lexing.Flow_Scalar_Token_Kind | Lexing.Node_Property_Kind =>
            P.Implicit_Key := True;
            if Before_Block_Item (P, E) then null; end if;
            P.Implicit_Key := False;
            P.Levels.Top.State := After_Implicit_Key'Access;
            return True;
         when Lexing.Alias =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Alias,
                               Target => From_String (P.Pool, Lexing.Short_Lexeme (P.L)));
            P.Current := Lexing.Next_Token (P.L);
            P.Levels.Top.State := After_Implicit_Key'Access;
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected mapping key): " &
              P.Current.Kind'Img;
      end case;
   end At_Block_Map_Key;

   function After_Implicit_Key (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean is
      pragma Unreferenced (E);
   begin
      if P.Current.Kind /= Lexing.Map_Value_Ind then
         raise Parser_Error with "Unexpected token (expected ':'): " &
           P.Current.Kind'Img;
      end if;
      P.Current := Lexing.Next_Token (P.L);
      P.Levels.Top.State := Before_Block_Map_Key'Access;
      P.Levels.Push
        ((State => Before_Block_Item'Access,
          Indentation => P.Levels.Top.Indentation));
      return False;
   end After_Implicit_Key;

   function Before_Block_Map_Value (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean is
      Indent : constant Lexing.Indentation_Type :=
        Lexing.Current_Indentation (P.L);
   begin
      case P.Current.Kind is
         when Lexing.Indentation =>
            if Indent < P.Levels.Top.Indentation then
                --  the value is allowed to be missing after an explicit key
                E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                   End_Position   => P.Current.End_Pos,
                                   Kind => Events.Scalar,
                                   Scalar_Attributes => (others => <>),
                                   Scalar_Style => Events.Plain,
                                   Value => Null_Content);
                P.Levels.Top.State := Before_Block_Map_Key'Access;
                return True;
            elsif Indent > P.Levels.Top.Indentation then
               raise Parser_Error with "Invalid indentation";
            end if;
         when Lexing.Document_End | Lexing.Directives_End | Lexing.Stream_End =>
            --  the value is allowed to be missing after an explicit key
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Scalar,
                               Scalar_Attributes => (others => <>),
                               Scalar_Style => Events.Plain,
                               Value => Null_Content);
            P.Levels.Top.State := Before_Block_Map_Key'Access;
            return True;
         when others =>
            raise Parser_Error with "Illegal token (expected new line) " &
              P.Current.Kind'Img;
      end case;
      P.Current := Lexing.Next_Token (P.L);
      case P.Current.Kind is
         when Lexing.Map_Value_Ind =>
            P.Levels.Top.State := Before_Block_Map_Key'Access;
            P.Levels.Push
              ((State => Before_Block_Item'Access,
                Indentation => P.Levels.Top.Indentation));
            P.Current := Lexing.Next_Token (P.L);
            return False;
         when Lexing.Map_Key_Ind =>
            --  the value is allowed to be missing after an explicit key
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Scalar,
                               Scalar_Attributes => (others => <>),
                               Scalar_Style => Events.Plain,
                               Value => Null_Content);
            P.Levels.Top.State := At_Block_Map_Key'Access;
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected mapping key): " &
              P.Current.Kind'Img;
      end case;
   end Before_Block_Map_Value;

   function Before_Flow_Item (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean is
      Attrs : Events.Attributes;
   begin
      loop
         case P.Current.Kind is
            when Lexing.Scalar_Token_Kind =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Scalar,
                                  Scalar_Attributes => Attrs,
                                  Scalar_Style => To_Style (P.Current.Kind),
                                  Value => Lexing.Current_Content (P.L));
               P.Current := Lexing.Next_Token (P.L);
               P.Levels.Pop;
               return True;
            when Lexing.Alias =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Alias,
                                  Target => From_String (P.Pool, Lexing.Short_Lexeme (P.L)));
               P.Current := Lexing.Next_Token (P.L);
               P.Levels.Pop;
               return True;
            when Lexing.Flow_Map_Start =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Mapping_Start,
                                  Collection_Attributes => Attrs,
                                  Collection_Style => Events.Flow);
               P.Levels.Top.State := After_Flow_Map_Sep'Access;
               P.Current := Lexing.Next_Token (P.L);
               return True;
            when Lexing.Flow_Seq_Start =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Sequence_Start,
                                  Collection_Attributes => Attrs,
                                  Collection_Style => Events.Flow);
               P.Levels.Top.State := After_Flow_Seq_Sep'Access;
               P.Current := Lexing.Next_Token (P.L);
               return True;
            when Lexing.Flow_Map_End | Lexing.Flow_Seq_End |
                 Lexing.Flow_Separator | Lexing.Map_Value_Ind =>
               E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                  End_Position   => P.Current.End_Pos,
                                  Kind => Events.Scalar,
                                  Scalar_Attributes => Attrs,
                                  Scalar_Style => Events.Plain,
                                  Value => Null_Content);
               P.Levels.Pop;
               return True;
            when Lexing.Anchor =>
               if Attrs.Anchor /= Null_Content then
                  raise Parser_Error with "Only one anchor allowed per element";
               end if;
               Attrs.Anchor := From_String (P.Pool, Lexing.Short_Lexeme (P.L));
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Tag_Handle =>
               if Attrs.Tag /= Null_Content then
                  raise Parser_Error with "Only one tag allowed per element";
               end if;
               Attrs.Tag := Parse_Tag (P);
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Verbatim_Tag =>
               if Attrs.Tag /= Null_Content then
                  raise Parser_Error with "Only one tag allowed per element";
               end if;
               Attrs.Tag := Lexing.Current_Content (P.L);
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Annotation =>
               Attrs.Annotations.Push
                 (Strings.From_String (P.Pool, Lexing.Short_Lexeme (P.L)));
               P.Current := Lexing.Next_Token (P.L);
            when others =>
               raise Parser_Error with
                 "Unexpected token (expected flow node): " & P.Current.Kind'Img;
         end case;
      end loop;
   end Before_Flow_Item;

   function After_Flow_Map_Key (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Map_Value_Ind =>
            P.Levels.Top.State := After_Flow_Map_Value'Access;
            P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
            P.Current := Lexing.Next_Token (P.L);
            return False;
         when Lexing.Flow_Separator | Lexing.Flow_Map_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Scalar,
                               Scalar_Attributes => (others => <>),
                               Scalar_Style => Events.Plain,
                               Value => Null_Content);
            P.Levels.Top.State := After_Flow_Map_Value'Access;
            return True;
         when others =>
            raise Parser_Error with "Unexpected token (expected ':'): " &
              P.Current.Kind'Img;
      end case;
   end After_Flow_Map_Key;

   function After_Flow_Map_Value (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Flow_Separator =>
            P.Levels.Top.State := After_Flow_Map_Sep'Access;
            P.Current := Lexing.Next_Token (P.L);
            return False;
         when Lexing.Flow_Map_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Mapping_End);
            P.Current := Lexing.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when Lexing.Flow_Scalar_Token_Kind | Lexing.Map_Key_Ind |
              Lexing.Anchor | Lexing.Alias | Lexing.Annotation |
              Lexing.Flow_Map_Start | Lexing.Flow_Seq_Start =>
            raise Parser_Error with "Missing ','";
         when others =>
            raise Parser_Error with "Unexpected token (expected ',' or '}'): " &
              P.Current.Kind'Img;
      end case;
   end After_Flow_Map_Value;

   function After_Flow_Seq_Item (P : in out Parser_Implementation'Class;
                                 E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Flow_Separator =>
            P.Levels.Top.State := After_Flow_Seq_Sep'Access;
            P.Current := Lexing.Next_Token (P.L);
            return False;
         when Lexing.Flow_Seq_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Sequence_End);
            P.Current := Lexing.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when Lexing.Flow_Scalar_Token_Kind | Lexing.Map_Key_Ind |
              Lexing.Anchor | Lexing.Alias | Lexing.Annotation |
              Lexing.Flow_Map_Start | Lexing.Flow_Seq_Start =>
            raise Parser_Error with "Missing ','";
         when others =>
            raise Parser_Error with "Unexpected token (expected ',' or ']'): " &
              P.Current.Kind'Img;
      end case;
   end After_Flow_Seq_Item;

   function After_Flow_Map_Sep (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Map_Key_Ind =>
            P.Current := Lexing.Next_Token (P.L);
         when Lexing.Flow_Map_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Mapping_End);
            P.Current := Lexing.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when others => null;
      end case;
      P.Levels.Top.State := After_Flow_Map_Key'Access;
      P.Levels.Push ((State => Before_Flow_Item'Access, Indentation => <>));
      return False;
   end After_Flow_Map_Sep;

   function After_Flow_Seq_Sep (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean is
   begin
      case P.Current.Kind is
         when Lexing.Flow_Seq_End =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Sequence_End);
            P.Current := Lexing.Next_Token (P.L);
            P.Levels.Pop;
            return True;
         when Lexing.Flow_Separator =>
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.Start_Pos,
                               Kind => Events.Scalar,
                               Scalar_Attributes => (others => <>),
                               Scalar_Style => Events.Plain,
                               Value => Null_Content);
            P.Current := Lexing.Next_Token (P.L);
            return True;
         when others =>
            P.Levels.Top.State := After_Flow_Seq_Item'Access;
            P.Levels.Push ((State => Before_Flow_Item'Access, others => <>));
            return False;
      end case;
   end After_Flow_Seq_Sep;

end Yaml.Parsing;
