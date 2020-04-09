--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Yaml.Presenter.Analysis;
with Yaml.Tags;

package body Yaml.Presenter is
   use type Text.Reference;

   Line_End : constant Character := Character'Val (10);

   procedure Configure (P : in out Instance; Max_Line_Length : Line_Length_Type;
                        Flow_Style : Flow_Style_Type) is
   begin
      P.Max_Line_Length := Max_Line_Length;
      P.Flow_Style := Flow_Style;
   end Configure;

   procedure Init (P : in out Instance) is
   begin
      P.Buffer_Pos := 1;
      P.Cur_Column := 1;
      P.Cur_Max_Column := P.Max_Line_Length;
      if P.Levels.Initialized then
         --  may occur when parser is re-used
         while P.Levels.Length > 0 loop
            P.Levels.Pop;
         end loop;
      else
         P.Levels := Level_Stacks.New_Stack (64);
      end if;
      P.Levels.Push (Level'(Before_Stream_Start, -2));
   end Init;

   procedure Set_Output (P : in out Instance;
                         D : not null Destination.Pointer) is
   begin
      Finalize (P);
      P.Dest := D;
      P.Buffer := new String (1 .. 8192);
      Init (P);
   end Set_Output;

   procedure Set_Output (P : in out Instance;
                         Buffer : not null Buffer_Type) is
   begin
      Finalize (P);
      P.Dest := null;
      P.Buffer := Buffer;
      Init (P);
   end Set_Output;

   procedure Flush (P : in out Instance) is
      use type Destination.Pointer;
   begin
      if P.Dest /= null and then P.Buffer_Pos > 1 then
         P.Dest.Write_Data (P.Buffer (P.Buffer'First .. P.Buffer_Pos - 1));
         P.Buffer_Pos := 1;
      end if;
   end Flush;

   procedure Free_S is new Ada.Unchecked_Deallocation
     (String, Buffer_Type);

   procedure Finalize (Object : in out Instance) is
      procedure Free_D is new Ada.Unchecked_Deallocation
        (Destination.Instance'Class, Destination.Pointer);
      use type Destination.Pointer;
   begin
      if Object.Dest /= null then
         Object.Flush;
         Free_D (Object.Dest);
         Free_S (Object.Buffer);
      end if;
   end Finalize;

   type Allowed_Styles is (All_Of_Them, No_Compact);

   procedure Put (P : in out Instance;
                  E : Event) is
      use type Analysis.Necessary_Quoting;

      subtype Chosen_Scalar_Style_Type is Scalar_Style_Type range
        Plain .. Folded;
      pragma Warnings (Off, "formal parameter ""In_Flow"" is not referenced");
      function Possibly_Block_Scalar_Style (Features : Analysis.Scalar_Features;
                                            In_Flow : Boolean)
                                            return Chosen_Scalar_Style_Type is
        (if Features.Single_Line_Length + P.Levels.Top.Indentation +
           (if Features.Unquoted_Single_Line then 2 else 4) <= P.Cur_Max_Column
         then Plain
         elsif Features.Max_Line_Length + P.Levels.Top.Indentation + 2 <=
           P.Cur_Max_Column then Literal elsif Features.Folding_Possible
         and then Features.Max_Word_Length + P.Levels.Top.Indentation + 2 <=
           P.Cur_Max_Column then Folded else Single_Quoted);
      pragma Warnings (On, "formal parameter ""In_Flow"" is not referenced");

      function Chosen_Scalar_Style (Features : Analysis.Scalar_Features;
                                    In_Flow : Boolean)
                                    return Chosen_Scalar_Style_Type is
        (case E.Scalar_Style is
            when Double_Quoted => Double_Quoted,
            when Single_Quoted | Literal | Folded =>
           (if Features.Quoting_Needed = Analysis.Double then
                 Double_Quoted else E.Scalar_Style),
            when Plain =>
           (case Features.Quoting_Needed is
               when Analysis.Double => Double_Quoted,
               when Analysis.Single => Single_Quoted,
               when Analysis.Only_In_Flow =>
              (if In_Flow then Single_Quoted else
                    Plain),
               when Analysis.None => Plain),
            when Any =>
           (case Features.Quoting_Needed is
               when Analysis.Double => Double_Quoted,
               when Analysis.Only_In_Flow =>
                  Scalar_Style_Type'Max
              (Possibly_Block_Scalar_Style (Features, In_Flow),
               (if In_Flow then Single_Quoted else Plain)),
               when Analysis.Single =>
                  Scalar_Style_Type'Max
              (Possibly_Block_Scalar_Style (Features, In_Flow),
               (Single_Quoted)),
               when Analysis.None =>
                  Possibly_Block_Scalar_Style (Features, In_Flow)));

      procedure Write (S : String) with
        Pre => (for all C of S => C /= Character'Val (10)) is
         use type Destination.Pointer;
      begin
         P.Cur_Column := P.Cur_Column + S'Length;
         if P.Buffer_Pos + S'Length - 1 > P.Buffer.all'Length then
            if P.Dest = null then
               raise Presenter_Error with
                 "output too long for destination string!";
            else
               P.Flush;
               if S'Length < P.Buffer'Length then
                  P.Dest.Write_Data (S);
                  return;
               end if;
            end if;
         end if;
         P.Buffer (P.Buffer_Pos .. P.Buffer_Pos + S'Length - 1) := S;
         P.Buffer_Pos := P.Buffer_Pos + S'Length;
      end Write;

      procedure Write (C : Character) with
        Pre => C /= Character'Val (10) is
         use type Destination.Pointer;
      begin
         P.Cur_Column := P.Cur_Column + 1;
         if P.Buffer_Pos > P.Buffer.all'Length then
            if P.Dest = null then
               raise Presenter_Error with
                 "output too long for destination string!";
            else
               P.Flush;
            end if;
         end if;
         P.Buffer (P.Buffer_Pos) := C;
         P.Buffer_Pos := P.Buffer_Pos + 1;
      end Write;

      procedure Next_Line (Count : Positive := 1) is
         use Ada.Strings.Fixed;
         use type Destination.Pointer;
      begin
         for I in 1 .. Count loop
            if P.Buffer_Pos > P.Buffer.all'Length then
               if P.Dest = null then
                  raise Presenter_Error with
                    "output too long for destination string!";
               else
                  P.Dest.Write_Data
                    (P.Buffer (P.Buffer'First .. P.Buffer_Pos - 1));
                  P.Buffer_Pos := 1;
               end if;
            end if;
            P.Buffer (P.Buffer_Pos) := Character'Val (10);
            P.Buffer_Pos := P.Buffer_Pos + 1;
         end loop;
         P.Cur_Column := 1;
         P.Cur_Max_Column := Positive'Max
           (P.Max_Line_Length / 2, P.Max_Line_Length - P.Levels.Top.Indentation);
         Write (P.Levels.Top.Indentation * ' ');
      end Next_Line;

      procedure Render_Tag (Tag : Text.Reference) is
         Yaml_Tag_Prefix : constant String := "tag:yaml.org,2002:";
      begin
         if Tag.Length > Yaml_Tag_Prefix'Length and then
           Tag.Value (1 .. Yaml_Tag_Prefix'Length) = Yaml_Tag_Prefix then
            Write ("!!");
            Write (Tag.Value (Yaml_Tag_Prefix'Length + 1 .. Tag.Length));
         else
            Write ("!<" & Tag & ">");
         end if;
      end Render_Tag;

      function Render_Inline_Properties (Props : Properties)
                                         return Boolean is
      begin
         return Wrote_Anything : Boolean := False do
            if Props.Anchor /= Text.Empty then
               Write (" &" & Props.Anchor);
               Wrote_Anything := True;
            end if;
            if Props.Tag /= Tags.Question_Mark then
               Write (' ');
               Render_Tag (Props.Tag);
               Wrote_Anything := True;
            end if;
         end return;
      end Render_Inline_Properties;

      function Render_Newline_Properties (Props : Properties)
                                          return Boolean is
      begin
         return Wrote_Anything : Boolean := False do
            if Props.Anchor /= Text.Empty then
               Write ('&' & Props.Anchor & ' ');
               Wrote_Anything := True;
            end if;
            if Props.Tag /= Tags.Question_Mark then
               Render_Tag (Props.Tag);
               Write (' ');
               Wrote_Anything := True;
            end if;
         end return;
      end Render_Newline_Properties;

      procedure Render_Single_Line_Double_Quoted
        with Pre => E.Kind = Scalar is
      begin
         Write ('"');
         for C of E.Content.Value loop
            case C is
               when '"' | '\'          => Write ('\' & C);
               when Character'Val (7)  => Write ("\a");
               when Character'Val (8)  => Write ("\b");
               when Character'Val (9)  => Write ("\t");
               when Character'Val (10) => Write ("\n");
               when Character'Val (13) => Write ("\r");
               when others             => Write (C);
            end case;
         end loop;
         Write ('"');
      end Render_Single_Line_Double_Quoted;

      procedure Render_Multi_Line_Double_Quoted
        with Pre => E.Kind = Scalar is
         Buffer : String
           (1 .. P.Cur_Max_Column + P.Levels.Top.Indentation)
           := (1 => '"', others => <>);
         Pos : Positive := 2;
         Recent_Was_Space : Boolean := False;
      begin
         for C of E.Content.Value loop
            case C is
               when '"' | '\' =>
                  Buffer (Pos) := '\';
                  Buffer (Pos + 1) := C;
                  Pos := Pos + 2;
                  Recent_Was_Space := False;
               when Character'Val (7) =>
                  Buffer (Pos .. Pos + 1) := "\a";
                  Pos := Pos + 2;
                  Recent_Was_Space := False;
               when Character'Val (8) =>
                  Buffer (Pos .. Pos + 1) := "\b";
                  Pos := Pos + 2;
                  Recent_Was_Space := False;
               when Character'Val (9) =>
                  Buffer (Pos .. Pos + 1) := "\t";
                  Pos := Pos + 2;
                  Recent_Was_Space := False;
               when Character'Val (10) =>
                  if not Recent_Was_Space and then
                    P.Cur_Column + Pos >= (P.Cur_Max_Column * 3) / 4 then
                     Write (Buffer (1 .. Pos - 1));
                     Next_Line (2);
                     Pos := 1;
                  else
                     Buffer (Pos .. Pos + 1) := "\n";
                     Pos := Pos + 2;
                  end if;
                  Recent_Was_Space := False;
               when Character'Val (13) =>
                  Buffer (Pos .. Pos + 1) := "\r";
                  Pos := Pos + 2;
                  Recent_Was_Space := False;
               when ' ' =>
                  if not Recent_Was_Space and then
                    P.Cur_Column + Pos >= (P.Cur_Max_Column * 3) / 4 then
                     Buffer (Pos .. Pos + 1) := " \";
                     Write (Buffer (1 .. Pos + 1));
                     Next_Line;
                     Pos := 1;
                     Recent_Was_Space := False;
                  else
                     Buffer (Pos) := ' ';
                     Pos := Pos + 1;
                     Recent_Was_Space := True;
                  end if;
               when others =>
                  Buffer (Pos) := C;
                  Pos := Pos + 1;
                  Recent_Was_Space := False;
            end case;
            if P.Cur_Column + Pos >= P.Cur_Max_Column - 1 then
               Buffer (Pos) := '\';
               Write (Buffer (1 .. Pos));
               Next_Line;
               Pos := 1;
               Recent_Was_Space := False;
            end if;
         end loop;
         Buffer (Pos) := '"';
         Write (Buffer (1 .. Pos));
      end Render_Multi_Line_Double_Quoted;

      procedure Render_Single_Quoted with Pre => E.Kind = Scalar is
      begin
         Write ("'TODO'");
      end Render_Single_Quoted;

      procedure Render_Single_Line_Plain
        with Pre => E.Kind = Scalar is
      begin
         Write (E.Content.Value);
      end Render_Single_Line_Plain;

      procedure Render_Multi_Line_Plain
        with Pre => E.Kind = Scalar is
         Word_Start : Positive := 1;
         Pos : Positive := 1;
         First : Boolean := True;
      begin
         while Pos < E.Content.Value.Data'Last loop
            if E.Content.Value.Data (Pos) = ' ' and then
              not (E.Content.Value.Data (Pos + 1) in ' ' | Character'Val (10)) then
               if P.Cur_Column + Pos - Word_Start <= P.Cur_Max_Column then
                  if First then
                     First := False;
                  else
                     Write (' ');
                  end if;
               else
                  Next_Line;
               end if;
               Write (E.Content.Value (Word_Start .. Pos - 1));
               Word_Start := Pos + 1;
               Pos := Pos + 2;
            else
               Pos := Pos + 1;
            end if;
         end loop;
         if P.Cur_Column + Pos - Word_Start <= P.Cur_Max_Column then
            if First then
               First := False;
            else
               Write (' ');
            end if;
         else
            Next_Line;
         end if;
         Write (E.Content.Value.Data (Word_Start .. E.Content.Value.Data'Last));
      end Render_Multi_Line_Plain;

      procedure Render_Literal_Scalar with Pre => E.Kind = Scalar is
         First_Char : Positive;
         Next_Newline : Positive := 1;
      begin
         if E.Content.Length = 0 then
            Write (" |-");
         else
            Write (" |");
            if E.Content (E.Content.Length) = Character'Val (10) then
               if E.Content.Length > 1 and then
                       E.Content (E.Content.Length - 1) = Character'Val (10)
               then
                  Write ('+');
               end if;
            else
               Write ('-');
            end if;
            if E.Content (1) = ' ' then
               Write ('2');
            end if;
            Outer : loop
               loop
                  Next_Line;
                  if Next_Newline > E.Content.Length then
                     exit Outer;
                  end if;
                  exit when E.Content (Next_Newline) /= Character'Val (10);
                  Next_Newline := Next_Newline + 1;
               end loop;
               First_Char := Next_Newline;
               while Next_Newline <= E.Content.Length and then
                 E.Content (Next_Newline) /= Character'Val (10) loop
                  Next_Newline := Next_Newline + 1;
               end loop;
               Write (E.Content.Value.Data (First_Char .. Next_Newline - 1));
               exit when Next_Newline > E.Content.Length or else
                 (Next_Newline = E.Content.Length and then
                  E.Content (Next_Newline) = Character'Val (10));
               Next_Newline := Next_Newline + 1;
            end loop Outer;
         end if;
      end Render_Literal_Scalar;

      procedure Render_Folded_Scalar with Pre => E.Kind = Scalar is
         Lookahead : Positive := 1;
         First_Char : Positive;
         Last_Space : Positive;
      begin
         if E.Content.Length = 0 then
            Write (" >-");
         else
            Write (" >");
            if E.Content (E.Content.Length) = Character'Val (10) then
               if E.Content.Length > 1 and then
                 E.Content (E.Content.Length - 1) = Character'Val (10)
               then
                  Write ('+');
               end if;
            else
               Write ('-');
            end if;
            if E.Content (1) = ' ' then
               Write ('2');
            end if;
            Outer : loop
               <<At_New_Line>>
               loop
                  if Lookahead > E.Content.Length then
                     exit Outer;
                  end if;
                  Next_Line;
                  exit when E.Content (Lookahead) /= Character'Val (10);
                  Lookahead := Lookahead + 1;
               end loop;
               First_Char := Lookahead;
               Last_Space := First_Char;
               loop
                  while not (E.Content (Lookahead) in ' ' | Character'Val (10)) loop
                     Lookahead := Lookahead + 1;
                     if Lookahead > E.Content.Length then
                        Write (E.Content.Value.Data (First_Char .. E.Content.Length));
                        exit Outer;
                     end if;
                  end loop;
                  if E.Content (Lookahead) = Character'Val (10) then
                     Write (E.Content.Value.Data (First_Char .. Lookahead - 1));
                     Lookahead := Lookahead + 1;
                     if Lookahead > E.Content.Length then
                        exit Outer;
                     end if;
                     Next_Line;
                     goto At_New_Line;
                  end if;
                  exit when Lookahead - First_Char >= P.Cur_Max_Column;
                  Last_Space := Lookahead;
                  Lookahead := Lookahead + 1;
               end loop;
               Write (E.Content.Value.Data (First_Char .. Last_Space - 1));
               Lookahead := Last_Space + 1;
            end loop Outer;
         end if;
      end Render_Folded_Scalar;

      procedure Render_Long_Scalar (In_Flow : Boolean;
                                    Features : Analysis.Scalar_Features)
        with Pre => E.Kind = Scalar is
         Style : constant Chosen_Scalar_Style_Type :=
           Chosen_Scalar_Style (Features, In_Flow);
      begin
         P.Levels.Push ((Position => <>,
                         Indentation => P.Levels.Top.Indentation + 2));
         case Style is
            when Literal => Render_Literal_Scalar;
            when Folded => Render_Folded_Scalar;
            when Double_Quoted =>
               Next_Line;
               Render_Multi_Line_Double_Quoted;
            when Single_Quoted =>
               Next_Line;
               Render_Single_Quoted;
            when Plain =>
               Next_Line;
               Render_Multi_Line_Plain;
         end case;
         P.Levels.Pop;
      end Render_Long_Scalar;

      procedure Render_Scalar (In_Flow : Boolean;
                               Features : Analysis.Scalar_Features)
        with Pre => E.Kind = Scalar is
      begin
         if P.Flow_Style = Canonical then
            if E.Scalar_Style in Literal | Folded then
               Render_Long_Scalar (True, Features);
            elsif P.Cur_Column + Features.Single_Line_Length + 3 <=
              P.Cur_Max_Column then
               Write (' ');
               Render_Single_Line_Double_Quoted;
            else
               P.Levels.Push ((Position => <>,
                         Indentation => P.Levels.Top.Indentation + 2));
               Next_Line;
               Render_Multi_Line_Double_Quoted;
               P.Levels.Pop;
            end if;
         elsif Features.Unquoted_Single_Line then
            if E.Scalar_Style in Any | Plain and then
              P.Cur_Column + Features.Single_Line_Length + 1 <=
                P.Cur_Max_Column then
               Write (' ');
               Render_Single_Line_Plain;
            elsif E.Scalar_Style = Double_Quoted and then
              P.Cur_Column + Features.Single_Line_Length + 3 <=
                P.Cur_Max_Column then
               Write (' ');
               Render_Single_Line_Double_Quoted;
            else
               Render_Long_Scalar (In_Flow, Features);
            end if;
         elsif E.Scalar_Style in Any | Double_Quoted and then
           P.Cur_Column + Features.Single_Line_Length + 3 <=
             P.Cur_Max_Column then
            Write (' ');
            Render_Single_Line_Double_Quoted;
         else
            Render_Long_Scalar (In_Flow, Features);
         end if;
      end Render_Scalar;

      type Node_Start_Descriptor is record
         Flow_Pos, Header_Pos, Implicit_Pos : Position_Type;
         Flow_Char : Character;
      end record;

      Mapping_Start_Descriptor : constant Node_Start_Descriptor :=
        (Flow_Pos => After_Flow_Map_Start,
         Header_Pos => After_Map_Header,
         Implicit_Pos => After_Implicit_Map_Start,
         Flow_Char => '{');
      Sequence_Start_Descriptor : constant Node_Start_Descriptor :=
        (Flow_Pos => After_Flow_Seq_Start,
         Header_Pos => After_Seq_Header,
         Implicit_Pos => After_Implicit_Seq_Start,
         Flow_Char => '[');

      procedure Start_Node (Inline : Boolean; Styles : Allowed_Styles;
                            Descriptor : Node_Start_Descriptor)
        with Pre => E.Kind in Mapping_Start | Sequence_Start is
      begin
         if (if Inline then Render_Inline_Properties (E.Collection_Properties)
             else Render_Newline_Properties (E.Collection_Properties)) then
            if Styles = No_Compact then
               if E.Collection_Style = Flow then
                  Next_Line;
                  Write (Descriptor.Flow_Char);
                  P.Levels.Push ((Position => Descriptor.Flow_Pos,
                                  Indentation => P.Levels.Top.Indentation + 2));
               else
                  P.Levels.Push ((Position => Descriptor.Implicit_Pos,
                                  Indentation => P.Levels.Top.Indentation + 2));
                  Next_Line;
               end if;
            elsif E.Collection_Style = Flow then
               if Inline then
                  Write (' ');
               end if;
               P.Levels.Push ((Position => Descriptor.Flow_Pos,
                               Indentation => P.Cur_Column - 1));
               Write (Descriptor.Flow_Char);
            else
               P.Levels.Push ((Position => Descriptor.Header_Pos,
                               Indentation => P.Levels.Top.Indentation + 2));
            end if;
         elsif Styles = No_Compact then
            if E.Collection_Style = Flow then
               Next_Line;
               Write (Descriptor.Flow_Char);
               P.Levels.Push ((Position => Descriptor.Flow_Pos,
                               Indentation => P.Levels.Top.Indentation + 2));
            else
               P.Levels.Push ((Position => Descriptor.Implicit_Pos,
                               Indentation => P.Levels.Top.Indentation + 2));
               Next_Line;
            end if;
         elsif E.Collection_Style = Flow then
            if Inline then
               Write (' ');
            end if;
            P.Levels.Push ((Position => Descriptor.Flow_Pos,
                            Indentation => P.Cur_Column - 1));
            Write (Descriptor.Flow_Char);
         else
            if Inline then
               Write (' ');
            end if;
            P.Levels.Push ((Position => Descriptor.Implicit_Pos,
                            Indentation => P.Cur_Column - 1));
         end if;
      end Start_Node;

      procedure Start_Flow_Node (Inline : Boolean;
                                 Descriptor : Node_Start_Descriptor)
        with Pre => E.Kind in Mapping_Start | Sequence_Start is
      begin
         if (if Inline then Render_Inline_Properties (E.Collection_Properties)
             else Render_Newline_Properties (E.Collection_Properties)) then
            null;
         end if;
         if Inline then
            Write (' ');
         end if;
         Write (Descriptor.Flow_Char);
         P.Levels.Push ((Position => Descriptor.Flow_Pos,
                         Indentation => P.Levels.Top.Indentation + 2));
      end Start_Flow_Node;

      procedure Render_Alias (Inline : Boolean) with Inline is
      begin
         if Inline then
            if P.Cur_Column + E.Target.Length + 2 <= P.Cur_Max_Column then
               Write (" *" & E.Target);
            else
               Next_Line;
               Write ("  *" & E.Target);
            end if;
         else
            Write ("*" & E.Target);
         end if;
      end Render_Alias;

      procedure Render_Annotation is
      begin
         if Render_Inline_Properties (E.Annotation_Properties) then null; end if;
         Write (" @" & E.Name);
         P.Levels.Push ((Position => After_Annotation_Name,
                         Indentation => P.Levels.Top.Indentation + 2));
      end Render_Annotation;

      procedure Start_Document is
      begin
         case E.Kind is
            when Stream_End =>
               Finalize (P);
            when Document_Start =>
               P.Levels.Top.Position := Before_Doc_End;
               if E.Version /= Text.Empty then
                  Write ("%YAML " & E.Version & Line_End & "---");
                  P.Levels.Push
                    ((Position => After_Directives_End, Indentation => 0));
               elsif E.Implicit_Start then
                  P.Levels.Push ((Position => After_Implicit_Doc_Start,
                                  Indentation => 0));
               else
                  Write ("---");
                  P.Levels.Push
                    ((Position => After_Directives_End, Indentation => 0));
               end if;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected document start or stream end): "
                 & E.Kind'Img;
         end case;
      end Start_Document;

      procedure Render_Scalar_Mapping_Key (In_Flow : Boolean)
        with Pre => E.Kind = Scalar is
      begin
         declare
            Features : constant Analysis.Scalar_Features :=
              Analysis.Features (E.Content.Value);
         begin
            if P.Flow_Style = Compact and then
              Features.Unquoted_Single_Line then
               if P.Cur_Column + Features.Single_Line_Length + 1 <=
                 P.Cur_Max_Column then
                  if Render_Newline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Single_Line_Plain;
                  Write (':');
                  P.Levels.Top.Position :=
                    (if In_Flow then After_Flow_Map_Key else
                        After_Implicit_Block_Map_Key);
               else
                  Write ('?');
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (In_Flow, Features);
                  if In_Flow then
                     Next_Line;
                     Write (':');
                     P.Levels.Top.Position := After_Flow_Map_Key;
                  else
                     P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  end if;
               end if;
            elsif P.Flow_Style = Compact and then
              P.Cur_Column + Features.Single_Line_Length + 3 <=
                P.Cur_Max_Column then
               if Render_Newline_Properties (E.Scalar_Properties) then null; end if;
               Render_Single_Line_Double_Quoted;
               Write (':');
               P.Levels.Top.Position :=
                 (if In_Flow then After_Flow_Map_Key else After_Explicit_Block_Map_Key);
            else
               Write ('?');
               if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
               Render_Scalar (In_Flow, Features);
               Next_Line;
               Write (':');
               P.Levels.Top.Position :=
                 (if In_Flow then After_Flow_Map_Key else After_Implicit_Block_Map_Key);
            end if;
         end;
      end Render_Scalar_Mapping_Key;

      procedure Start_Block_Key_Value_Pair is
      begin
         case E.Kind is
            when Scalar =>
               Next_Line;
               Render_Scalar_Mapping_Key (False);
            when Mapping_Start =>
               P.Levels.Top.Position := After_Explicit_Block_Map_Key;
               Next_Line;
               Write ('?');
               Start_Node (True, No_Compact, Mapping_Start_Descriptor);
            when Sequence_Start =>
               P.Levels.Top.Position := After_Implicit_Block_Map_Key;
               Next_Line;
               Write ('?');
               Start_Node (True, No_Compact, Sequence_Start_Descriptor);
            when Alias =>
               if P.Cur_Column + E.Target.Length + 3 <=
                 P.Cur_Max_Column then
                  Render_Alias (False);
                  Write (" :");
                  P.Levels.Top.Position := After_Implicit_Block_Map_Key;
               else
                  Write ("?");
                  Render_Alias (True);
                  P.Levels.Top.Position := After_Explicit_Block_Map_Key;
               end if;
            when Mapping_End =>
               --  empty mapping must be flow-style
               Write (" {}");
               P.Levels.Pop;
            when Annotation_Start => Render_Annotation;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected mapping key): " & E.Kind'Img;
         end case;
      end Start_Block_Key_Value_Pair;

      procedure Start_Flow_Key_Value_Pair with Pre => E.Kind /= Mapping_End is
      begin
         if P.Flow_Style = Canonical then
            Next_Line;
         end if;
         case E.Kind is
            when Scalar => Render_Scalar_Mapping_Key (True);
            when Mapping_Start =>
               if P.Flow_Style = Compact then
                  Write (" ?");
               else
                  Write ('?');
               end if;
               Next_Line;
               Write (':');
               P.Levels.Top.Position := After_Flow_Map_Key;
               Start_Flow_Node (True, Mapping_Start_Descriptor);
            when Sequence_Start =>
               if P.Flow_Style = Compact then
                  Write (" ?");
               else
                  Write ('?');
               end if;
               Write (':');
               P.Levels.Top.Position := After_Flow_Map_Key;
               Start_Flow_Node (True, Sequence_Start_Descriptor);
            when Alias =>
               if P.Flow_Style = Compact and then
                 P.Cur_Column + E.Target.Length + 3 <= P.Cur_Max_Column then
                  Render_Alias (False);
                  Write (" :");
                  P.Levels.Top.Position := After_Flow_Map_Key;
               else
                  if P.Flow_Style = Compact then
                     Next_Line;
                  end if;
                  Write ('?');
                  Render_Alias (True);
                  Write (':');
                  P.Levels.Top.Position := After_Flow_Map_Key;
               end if;
            when Annotation_Start => Render_Annotation;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected mapping key): " & E.Kind'Img;
         end case;
      end Start_Flow_Key_Value_Pair;

      procedure Start_Block_Sequence_Item is
      begin
         case E.Kind is
            when Scalar =>
               Next_Line;
               Write ('-');
               if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
               Render_Scalar (False, Analysis.Features (E.Content.Value));
               P.Levels.Top.Position := After_Block_Seq_Item;
            when Mapping_Start =>
               Next_Line;
               Write ('-');
               P.Levels.Top.Position := After_Block_Seq_Item;
               Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
            when Sequence_Start =>
               Next_Line;
               Write ('-');
               P.Levels.Top.Position := After_Block_Seq_Item;
               Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
            when Alias =>
               Next_Line;
               Write ('-');
               Render_Alias (True);
               P.Levels.Top.Position := After_Block_Seq_Item;
            when Sequence_End =>
               Write (" []");
               P.Levels.Pop;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected sequence item): " & E.Kind'Img;
         end case;
      end Start_Block_Sequence_Item;

      procedure Start_Flow_Sequence_Item (After_Comma : Boolean;
                                          After_Position : Position_Type;
                                          End_Token : Event_Kind;
                                          End_Char : Character;
                                          Force_Inline : Boolean := False) is
         Inline : Boolean;
      begin
         if P.Flow_Style = Canonical and then not Force_Inline then
            Next_Line;
            Inline := True;
         else
            Inline := After_Comma;
         end if;
         case E.Kind is
            when Scalar =>
               if Inline then
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
               else
                  if Render_Newline_Properties (E.Scalar_Properties) then null; end if;
               end if;
               declare
                  Features : constant Analysis.Scalar_Features :=
                    Analysis.Features (E.Content.Value);
               begin
                  if Features.Unquoted_Single_Line then
                     if P.Cur_Column + Features.Single_Line_Length + 2 <=
                       P.Cur_Max_Column then
                        if Inline then
                           Write (' ');
                        end if;
                        Render_Single_Line_Plain;
                     else
                        Render_Scalar (True, Features);
                     end if;
                  elsif P.Cur_Column + Features.Single_Line_Length + 4 <=
                    P.Cur_Max_Column then
                     if Inline then
                        Write (' ');
                     end if;
                     Render_Single_Line_Double_Quoted;
                  else
                     Render_Scalar (True, Features);
                  end if;
               end;
               P.Levels.Top.Position := After_Position;
            when Mapping_Start =>
               Next_Line;
               P.Levels.Top.Position := After_Position;
               Start_Flow_Node (False, Mapping_Start_Descriptor);
            when Sequence_Start =>
               Next_Line;
               P.Levels.Top.Position := After_Position;
               Start_Flow_Node (False, Sequence_Start_Descriptor);
            when Alias =>
               Render_Alias (Inline);
               P.Levels.Top.Position := After_Position;
            when others =>
               if E.Kind = End_Token then
                  Write (End_Char);
                  P.Levels.Pop;
               else
                  raise Presenter_Error with
                    "Unexpected event (expected sequence item): " & E.Kind'Img;
               end if;
         end case;
      end Start_Flow_Sequence_Item;

   begin
      case P.Levels.Top.Position is
         when Before_Stream_Start =>
            if E.Kind /= Stream_Start then
               raise Presenter_Error with "missing Stream_Start event; got " &
                 E.Kind'Img;
            end if;
            P.Levels.Top.Position := Before_Doc_Start;
         when After_Stream_End =>
            raise Presenter_Error with "Unexpected event after stream end: " &
              E.Kind'Img;
         when Before_Doc_Start => Start_Document;
         when After_Implicit_Doc_Start =>
            case E.Kind is
               when Scalar =>
                  P.Levels.Top.Position := Before_Doc_End;
                  --  scalars at root level *must* have `---`
                  Write ("---");
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Long_Scalar (False, Analysis.Features (E.Content.Value));
               when Mapping_Start =>
                  P.Levels.Top.Position := Before_Doc_End;
                  if E.Collection_Style /= Flow and then
                    (not Is_Empty (E.Collection_Properties)) then
                     Write ("---");
                     Start_Node (True, No_Compact, Mapping_Start_Descriptor);
                  else
                     Start_Node (False, All_Of_Them, Mapping_Start_Descriptor);
                  end if;
               when Sequence_Start =>
                  P.Levels.Top.Position := Before_Doc_End;
                  if E.Collection_Style /= Flow and then
                    (not Is_Empty (E.Collection_Properties)) then
                     Write ("---");
                     Start_Node (True, No_Compact, Sequence_Start_Descriptor);
                  else
                     Start_Node (False, All_Of_Them, Sequence_Start_Descriptor);
                  end if;
               when Alias =>
                  P.Levels.Top.Position := Before_Doc_End;
                  Next_Line;
                  Render_Alias (False);
               when Annotation_Start => Render_Annotation;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected node start): " & E.Kind'Img;
            end case;
         when After_Directives_End =>
            case E.Kind is
               when Scalar =>
                  P.Levels.Top.Position := Before_Doc_End;
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Long_Scalar (False, Analysis.Features (E.Content.Value));
               when Mapping_Start =>
                  P.Levels.Top.Position := Before_Doc_End;
                  Start_Node (True, No_Compact, Mapping_Start_Descriptor);
               when Sequence_Start =>
                     Start_Node (True, No_Compact, Sequence_Start_Descriptor);
               when Alias =>
                  P.Levels.Top.Position := Before_Doc_End;
                  Next_Line;
                  Render_Alias (False);
               when Annotation_Start => Render_Annotation;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected node start): " & E.Kind'Img;
            end case;
         when Before_Doc_End =>
            if E.Kind /= Document_End then
               raise Presenter_Error with
                 "Unexpected event (expected document end): " & E.Kind'Img;
            end if;
            if E.Implicit_End then
               Next_Line;
               P.Levels.Top.Position := After_Implicit_Doc_End;
            else
               Next_Line;
               Write ("...");
               Next_Line;
               P.Levels.Top.Position := Before_Doc_Start;
            end if;
         when After_Implicit_Doc_End =>
            case E.Kind is
               when Document_Start =>
                  if E.Version /= Text.Empty then
                     Write ("...");
                     Next_Line;
                  end if;
                  Start_Document;
               when Stream_End =>
                  P.Levels.Top.Position := After_Stream_End;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected document start or stream end): " &
                    E.Kind'Img;
            end case;
         when After_Implicit_Map_Start =>
            case E.Kind is
               when Scalar =>
                  Render_Scalar_Mapping_Key (False);
               when Mapping_Start =>
                  P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  Write ('?');
                  Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
               when Sequence_Start =>
                  P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  Write ('?');
                  Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
               when Alias =>
                  if P.Cur_Column + E.Target.Length + 3 <= P.Cur_Max_Column then
                     Render_Alias (False);
                     Write (" :");
                     P.Levels.Top.Position := After_Implicit_Block_Map_Key;
                  else
                     Write ('?');
                     Render_Alias (True);
                     P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  end if;
               when Mapping_End =>
                  --  empty mapping must be flow-style
                  Write ("{}");
                  P.Levels.Pop;
               when Annotation_Start => Render_Annotation;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping key): " & E.Kind'Img;
            end case;
         when After_Map_Header => Start_Block_Key_Value_Pair;
         when After_Explicit_Block_Map_Key =>
            Next_Line;
            Write (':');
            case E.Kind is
               when Scalar =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (False, Analysis.Features (E.Content.Value));
               when Mapping_Start =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
               when Sequence_Start =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
               when Alias =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  Render_Alias (True);
               when Annotation_Start =>
                  P.Levels.Top.Position := After_Implicit_Block_Map_Key;
                  Render_Annotation;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Implicit_Block_Map_Key =>
            case E.Kind is
               when Scalar =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (False, Analysis.Features (E.Content.Value));
               when Mapping_Start =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  Start_Node (True, No_Compact, Mapping_Start_Descriptor);
               when Sequence_Start =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  Start_Node (True, No_Compact, Sequence_Start_Descriptor);
               when Alias =>
                  P.Levels.Top.Position := After_Block_Map_Value;
                  Render_Alias (True);
               when Annotation_Start => Render_Annotation;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Block_Map_Value =>
            if E.Kind = Mapping_End then
               P.Levels.Pop;
            else
               Start_Block_Key_Value_Pair;
            end if;
         when After_Seq_Header => Start_Block_Sequence_Item;
         when After_Implicit_Seq_Start =>
            case E.Kind is
               when Scalar =>
                  Write ('-');
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (False, Analysis.Features (E.Content.Value));
                  P.Levels.Top.Position := After_Block_Seq_Item;
               when Mapping_Start =>
                  Write ('-');
                  P.Levels.Top.Position := After_Block_Seq_Item;
                  Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
               when Sequence_Start =>
                  Write ('-');
                  P.Levels.Top.Position := After_Block_Seq_Item;
                  Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
               when Alias =>
                  Write ('-');
                  Render_Alias (True);
                  P.Levels.Top.Position := After_Block_Seq_Item;
               when Annotation_Start =>
                  Write ('-');
                  Render_Annotation;
               when Sequence_End =>
                  Write ("[]");
                  P.Levels.Pop;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected sequence item): " & E.Kind'Img;
            end case;
         when After_Block_Seq_Item =>
            if E.Kind = Sequence_End then
               P.Levels.Pop;
            else
               Start_Block_Sequence_Item;
            end if;
         when After_Flow_Map_Start =>
            if E.Kind = Mapping_End then
               --  make sure `{}` is rendered even in canonical style
               Write ('}');
               P.Levels.Pop;
            else
               Start_Flow_Key_Value_Pair;
            end if;
         when After_Flow_Map_Key =>
            case E.Kind is
               when Scalar =>
                  P.Levels.Top.Position := After_Flow_Map_Value;
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (True, Analysis.Features (E.Content.Value));
               when Mapping_Start =>
                  P.Levels.Top.Position := After_Flow_Map_Value;
                  Start_Flow_Node (True, Mapping_Start_Descriptor);
               when Sequence_Start =>
                  P.Levels.Top.Position := After_Flow_Map_Value;
                  Start_Flow_Node (True, Sequence_Start_Descriptor);
               when Alias => Render_Alias (True);
               when Annotation_Start => Render_Annotation;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Flow_Map_Value =>
            if E.Kind = Mapping_End then
               P.Levels.Pop;
               if P.Flow_Style = Canonical then
                  Next_Line;
               end if;
               Write ('}');
            else
               Write (',');
               Start_Flow_Key_Value_Pair;
            end if;
         when After_Flow_Seq_Start =>
            Start_Flow_Sequence_Item (False, After_Flow_Seq_Item,
                                      Sequence_End, ']');
         when After_Flow_Seq_Item =>
            if E.Kind = Sequence_End then
               P.Levels.Pop;
               if P.Flow_Style = Canonical then
                  Next_Line;
               end if;
               Write (']');
            else
               Write (',');
               Start_Flow_Sequence_Item (True, After_Flow_Seq_Item,
                                        Sequence_End, ']');
            end if;
         when After_Annotation_Name =>
            if E.Kind = Annotation_End then
               P.Levels.Pop;
            else
               Write ('(');
               Start_Flow_Sequence_Item (False, After_Annotation_Param,
                                         Annotation_End, ')', True);
            end if;
         when After_Annotation_Param =>
            Start_Flow_Sequence_Item (True, After_Annotation_Param,
                                      Annotation_End, ')', True);
      end case;
   end Put;

   procedure Consume (P : in out Instance; S : in out Stream.Instance) is
      Cur : Event := Stream.Next (S);
   begin
      loop
         P.Put (Cur);
         exit when Cur.Kind = Stream_End;
         Cur := Stream.Next (S);
      end loop;
   end Consume;

end Yaml.Presenter;
