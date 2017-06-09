package body Yada.Parsing is
   use type Lexing.Token_Kind;
   use Yada.Strings;

   procedure Init (P : not null access Parser_Implementation) with Inline is
   begin
      P.Levels.Push ((State => At_Stream_Start'Access, Indentation => -2));
   end Init;

   procedure Parse (P : in out Parser; Input : Sources.Source_Access) is
      Pool : String_Pool;
   begin
      Create (Pool, 8092);
      declare
         PI : constant not null access Parser_Implementation :=
           new Parser_Implementation'(Streams.Stream_Implementation with
              L => <>, Pool => Pool, Levels => Level_Stacks.New_Stack (32),
              Current => <>, Cached => <>, Implicit_Key => False);
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
              Current => <>, Cached => <>, Implicit_Key => False);
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


   function At_Stream_Start (P : in out Parser_Implementation'Class;
                             E : out Events.Event) return Boolean is
   begin
      P.Levels.Top.all := (State => At_Stream_End'Access, Indentation => -2);
      P.Levels.Push ((State => Before_Doc'Access, Indentation => -1));
      E := Events.Event'(Kind => Events.Stream_Start,
                         Start_Position => (Line => 1, Column => 1, Index => 1),
                         End_Position => (Line => 1, Column => 1, Index => 1));

      P.Current := Lexing.Next_Token (P.L);
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
            when Lexing.Document_End => null;
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
               raise Parser_Error with "Not implemented: tag directives";
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
            P.Levels.Top.State := Before_Doc'Access;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected document end): " & P.Current.Kind'Img;
      end case;
      return True;
   end Before_Doc_End;

   function Before_Block_Item (P : in out Parser_Implementation'Class;
                               E : out Events.Event) return Boolean is
      Collection_Attrs : Events.Attributes := (others => Null_Content);
      Attrs : Events.Attributes := (others => Null_Content);

      function Event_Kind (T : Lexing.Scalar_Token_Kind)
                           return Events.Scalar_Style_Type is
        (case T is
            when Lexing.Plain_Scalar => Events.Plain,
            when Lexing.Single_Quoted_Scalar => Events.Single_Quoted,
            when Lexing.Double_Quoted_Scalar => Events.Double_Quoted,
            when Lexing.Literal_Scalar => Events.Literal,
            when Lexing.Folded_Scalar => Events.Folded) with Inline;
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
                                     Value => From_String (P.Pool, ""));
                  P.Levels.Pop;
                  return True;
               elsif P.Implicit_Key then
                  raise Parser_Error with
                    "Implicit mapping key must occur on single line";
               end if;
               Collection_Attrs := Attrs;
               Attrs := (others => Null_Content);
               P.Current := Lexing.Next_Token (P.L);
            when Lexing.Anchor | Lexing.Alias | Lexing.Annotation =>
               raise Parser_Error with "Not implemented: node properties";
            when Lexing.Scalar_Token_Kind =>
               declare
                  Indent : constant Indentation_Type :=
                    Lexing.Recent_Indentation (P.L);
               begin
                  E := Events.Event'(Start_Position => P.Current.Start_Pos,
                                     End_Position   => P.Current.End_Pos,
                                     Kind => Events.Scalar,
                                     Scalar_Attributes => Attrs,
                                     Scalar_Style => Event_Kind (P.Current.Kind),
                                     Value => Lexing.Current_Content (P.L));
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
                                   Scalar_Attributes => (others => Null_Content),
                                   Scalar_Style => Events.Plain,
                                   Value => From_String (P.Pool, ""));
                P.Levels.Top.State := Before_Block_Map_Key'Access;
                return True;
            elsif Indent > P.Levels.Top.Indentation then
               raise Parser_Error with "Invalid indentation";
            end if;
            P.Current := Lexing.Next_Token (P.L);
         when Lexing.Document_End | Lexing.Directives_End | Lexing.Stream_End =>
            --  the value is allowed to be missing after an explicit key
            E := Events.Event'(Start_Position => P.Current.Start_Pos,
                               End_Position   => P.Current.End_Pos,
                               Kind => Events.Scalar,
                               Scalar_Attributes => (others => Null_Content),
                               Scalar_Style => Events.Plain,
                               Value => From_String (P.Pool, ""));
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
                               Scalar_Attributes => (others => Null_Content),
                               Scalar_Style => Events.Plain,
                               Value => From_String (P.Pool, ""));
            P.Levels.Top.State := At_Block_Map_Key'Access;
            return True;
         when others =>
            raise Parser_Error with
              "Unexpected token (expected mapping key): " &
              P.Current.Kind'Img;
      end case;
   end Before_Block_Map_Value;
end Yada.Parsing;
