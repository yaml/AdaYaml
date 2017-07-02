--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Yaml.Strings;
with Yaml.Presenting.Analysis;

package body Yaml.Presenting is
   use Yaml.Strings;
   use type Events.Event_Kind;
   use type Events.Collection_Style_Type;

   Line_End : constant Character := Character'Val (10);
   Max_Line_Length : constant := 80;

   procedure Init (P : in out Presenter) is
   begin
      P.Buffer_Pos := 1;
      P.Cur_Column := 1;
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

   procedure Set_Output (P : in out Presenter;
                         D : not null Destinations.Destination_Access) is
   begin
      Finalize (P);
      P.Dest := D;
      P.Buffer := new String (1 .. 8192);
      Init (P);
   end Set_Output;

   procedure Set_Output (P : in out Presenter;
                         Buffer : not null Buffer_Type) is
   begin
      Finalize (P);
      P.Dest := null;
      P.Buffer := Buffer;
      Init (P);
   end Set_Output;

   procedure Flush (P : in out Presenter) is
      use type Destinations.Destination_Access;
   begin
      if P.Dest /= null and then P.Buffer_Pos > 1 then
         P.Dest.Write_Data (P.Buffer (P.Buffer'First .. P.Buffer_Pos - 1));
         P.Buffer_Pos := 1;
      end if;
   end Flush;

   procedure Free_S is new Ada.Unchecked_Deallocation (String, Buffer_Type);

   procedure Finalize (Object : in out Presenter) is
      procedure Free_D is new Ada.Unchecked_Deallocation
        (Destinations.Destination'Class, Destinations.Destination_Access);
      use type Destinations.Destination_Access;
   begin
      if Object.Dest /= null then
         Object.Flush;
         Free_D (Object.Dest);
         Free_S (Object.Buffer);
      end if;
   end Finalize;

   type Allowed_Styles is (All_Of_Them, No_Compact);

   procedure Put (P : in out Presenter;
                  E : Events.Event) is
      use type Analysis.Necessary_Quoting;

      subtype Chosen_Scalar_Style_Type is Events.Scalar_Style_Type range
        Events.Plain .. Events.Folded;

      function Possibly_Block_Scalar_Style (Features : Analysis.Scalar_Features;
                                            In_Flow : Boolean)
                                            return Chosen_Scalar_Style_Type is
        (if Features.Single_Line_Length + P.Levels.Top.Indentation +
           (if Features.Unquoted_Single_Line then 2 else 4) <= Max_Line_Length
         then Events.Plain
         elsif Features.Max_Line_Length + P.Levels.Top.Indentation + 2 <=
           Max_Line_Length then Events.Literal
         elsif Features.Folding_Possible and then
         Features.Max_Word_Length + P.Levels.Top.Indentation + 2 <=
           Max_Line_Length then Events.Folded
         else Events.Single_Quoted);

      function Chosen_Scalar_Style (Features : Analysis.Scalar_Features;
                                    In_Flow : Boolean)
                                    return Chosen_Scalar_Style_Type is
        (case E.Scalar_Style is
            when Events.Double_Quoted => Events.Double_Quoted,
            when Events.Single_Quoted | Events.Literal | Events.Folded =>
           (if Features.Quoting_Needed = Analysis.Double then
                 Events.Double_Quoted else E.Scalar_Style),
            when Events.Plain =>
           (case Features.Quoting_Needed is
               when Analysis.Double => Events.Double_Quoted,
               when Analysis.Single => Events.Single_Quoted,
               when Analysis.Only_In_Flow =>
              (if In_Flow then Events.Single_Quoted else
                    Events.Plain),
               when Analysis.None => Events.Plain),
            when Events.Any =>
           (case Features.Quoting_Needed is
               when Analysis.Double => Events.Double_Quoted,
               when Analysis.Only_In_Flow =>
                  Events.Scalar_Style_Type'Max
              (Possibly_Block_Scalar_Style (Features, In_Flow),
               (if In_Flow then Events.Single_Quoted else Events.Plain)),
               when Analysis.Single =>
                  Events.Scalar_Style_Type'Max
              (Possibly_Block_Scalar_Style (Features, In_Flow),
               (Events.Single_Quoted)),
               when Analysis.None =>
                  Possibly_Block_Scalar_Style (Features, In_Flow)));

      procedure Write (S : String) with
        Pre => (for all C of S => C /= Character'Val (10)) is
         use type Yaml.Destinations.Destination_Access;
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
         use type Yaml.Destinations.Destination_Access;
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
         use type Destinations.Destination_Access;
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
         Write (P.Levels.Top.Indentation * ' ');
      end Next_Line;

      function Render_Inline_Properties (Props : Events.Properties)
                                         return Boolean is
      begin
         return Wrote_Anything : Boolean := False do
            if Props.Anchor /= Null_Content then
               Write (" &" & Props.Anchor.Get);
               Wrote_Anything := True;
            end if;
            if Props.Tag /= Null_Content then
               Write (" !<" & Props.Tag.Get & '>');
               Wrote_Anything := True;
            end if;
            for Index in 1 .. Props.Annotations.Length loop
               Write (" @" & Props.Annotations.Element (Index).Get);
               Wrote_Anything := True;
            end loop;
         end return;
      end Render_Inline_Properties;

      function Render_Newline_Properties (Props : Events.Properties)
                                          return Boolean is
      begin
         return Wrote_Anything : Boolean := False do
            if Props.Anchor /= Null_Content then
               Write ('&' & Props.Anchor.Get & ' ');
               Wrote_Anything := True;
            end if;
            if Props.Tag /= Null_Content then
               Write ("!<" & Props.Tag.Get & "> ");
               Wrote_Anything := True;
            end if;
            for Index in 1 .. Props.Annotations.Length loop
               Write ('@' & Props.Annotations.Element (Index).Get & ' ');
               Wrote_Anything := True;
            end loop;
         end return;
      end Render_Newline_Properties;

      procedure Render_Single_Line_Double_Quoted
        with Pre => E.Kind = Events.Scalar is
      begin
         Write (" """);
         for C of E.Value.Get loop
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
        with Pre => E.Kind = Events.Scalar is
         This_Max_Line_Length : constant Positive := Positive'Max
           (Max_Line_Length / 2, Max_Line_Length - P.Levels.Top.Indentation);
         Buffer : String (1 .. This_Max_Line_Length + P.Levels.Top.Indentation)
           := (1 => '"', others => <>);
         Pos : Positive := 2;
         Recent_Was_Space : Boolean := False;
      begin
         for C of E.Value.Get loop
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
                    Pos >= (This_Max_Line_Length * 3) / 4 then
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
                    Pos >= (This_Max_Line_Length * 3) / 4 then
                     Write (Buffer (1 .. Pos - 1));
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
            if Pos >= This_Max_Line_Length - 1 then
               Buffer (Pos) := '\';
               Write (Buffer (1 .. Pos));
               Next_Line;
               Pos := 1;
               Recent_Was_Space := False;
            end if;
         end loop;
      end Render_Multi_Line_Double_Quoted;

      procedure Render_Single_Quoted with Pre => E.Kind = Events.Scalar is
      begin
         Write ("'TODO'");
      end Render_Single_Quoted;

      procedure Render_Single_Line_Plain
        with Pre => E.Kind = Events.Scalar is
      begin
         Write (E.Value.Get);
      end Render_Single_Line_Plain;

      procedure Render_Multi_Line_Plain
        with Pre => E.Kind = Events.Scalar is
         This_Max_Line_Length : constant Positive := Positive'Max
           (Max_Line_Length / 2, Max_Line_Length - P.Levels.Top.Indentation);
         Word_Start : Positive := 1;
         Pos : Positive := 1;
         First : Boolean := True;
      begin
         while Pos < E.Value.Get.Data'Last loop
            if E.Value.Get.Data (Pos) = ' ' and then
              not (E.Value.Get.Data (Pos + 1) in ' ' | Character'Val (10)) then
               if P.Cur_Column + Pos - Word_Start <= This_Max_Line_Length then
                  if First then
                     First := False;
                  else
                     Write (' ');
                  end if;
               else
                  Next_Line;
               end if;
               Write (E.Value.Get (Word_Start .. Pos - 1));
               Word_Start := Pos + 1;
               Pos := Pos + 2;
            else
               Pos := Pos + 1;
            end if;
         end loop;
         if P.Cur_Column + Pos - Word_Start <= This_Max_Line_Length then
            if First then
               First := False;
            else
               Write (' ');
            end if;
         else
            Next_Line;
         end if;
         Write (E.Value.Get (Word_Start .. E.Value.Get.Data'Last));
      end Render_Multi_Line_Plain;

      procedure Render_Literal_Scalar with Pre => E.Kind = Events.Scalar is
      begin
         Write (" |" & E.Value.Get);
      end Render_Literal_Scalar;

      procedure Render_Folded_Scalar with Pre => E.Kind = Events.Scalar is
      begin
         Write (" >" & E.Value.Get);
      end Render_Folded_Scalar;

      procedure Render_Long_Scalar (In_Flow : Boolean;
                                    Features : Analysis.Scalar_Features)
        with Pre => E.Kind = Events.Scalar is
         Style : constant Chosen_Scalar_Style_Type :=
           Chosen_Scalar_Style (Features, In_Flow);
      begin
         P.Levels.Push ((Position => <>,
                         Indentation => P.Levels.Top.Indentation + 2));
         case Style is
            when Events.Literal => Render_Literal_Scalar;
            when Events.Folded => Render_Folded_Scalar;
            when Events.Double_Quoted =>
               Next_Line;
               Render_Multi_Line_Double_Quoted;
            when Events.Single_Quoted =>
               Next_Line;
               Render_Single_Quoted;
            when Events.Plain =>
               Next_Line;
               Render_Multi_Line_Plain;
         end case;
         P.Levels.Pop;
      end Render_Long_Scalar;

      procedure Render_Scalar (In_Flow : Boolean;
                               Features : Analysis.Scalar_Features)
        with Pre => E.Kind = Events.Scalar is
         use type Events.Scalar_Style_Type;
      begin
         if Features.Unquoted_Single_Line then
            if E.Scalar_Style in Events.Any | Events.Plain and then
              P.Cur_Column + Features.Single_Line_Length + 1 <=
                Max_Line_Length then
               Write (' ');
               Render_Single_Line_Plain;
            elsif E.Scalar_Style = Events.Double_Quoted and then
              P.Cur_Column + Features.Single_Line_Length + 3 <=
                Max_Line_Length then
               Write (' ');
               Render_Single_Line_Double_Quoted;
            else
               Render_Long_Scalar (In_Flow, Features);
            end if;
         elsif E.Scalar_Style in Events.Any | Events.Double_Quoted and then
           P.Cur_Column + Features.Single_Line_Length + 3 <=
             Max_Line_Length then
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
        with Pre => E.Kind in Events.Mapping_Start | Events.Sequence_Start is
      begin
         if (if Inline then Render_Inline_Properties (E.Collection_Properties)
             else Render_Newline_Properties (E.Collection_Properties)) then
            if Styles = No_Compact then
               if E.Collection_Style = Events.Flow then
                  P.Levels.Push ((Position => Descriptor.Flow_Pos,
                                  Indentation => P.Levels.Top.Indentation + 2));
                  Next_Line;
                  Write (Descriptor.Flow_Char);
               else
                  P.Levels.Push ((Position => Descriptor.Implicit_Pos,
                                  Indentation => P.Levels.Top.Indentation + 2));
                  Next_Line;
               end if;
            elsif E.Collection_Style = Events.Flow then
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
            if E.Collection_Style = Events.Flow then
               P.Levels.Push ((Position => Descriptor.Flow_Pos,
                               Indentation => P.Levels.Top.Indentation + 2));
               Next_Line;
               Write (Descriptor.Flow_Char);
            else
               P.Levels.Push ((Position => Descriptor.Implicit_Pos,
                               Indentation => P.Levels.Top.Indentation + 2));
               Next_Line;
            end if;
         elsif E.Collection_Style = Events.Flow then
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
        with Pre => E.Kind in Events.Mapping_Start | Events.Sequence_Start is
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
            if P.Cur_Column + E.Target.Get.Data'Length + 2 <= Max_Line_Length then
               Write (" *" & E.Target.Get);
            else
               Next_Line;
               Write ("  *" & E.Target.Get);
            end if;
         else
            Write ("*" & E.Target.Get);
         end if;
      end Render_Alias;

      procedure Start_Document is
      begin
         case E.Kind is
            when Events.Stream_End =>
               Finalize (P);
            when Events.Document_Start =>
               P.Levels.Top.Position := Before_Doc_End;
               if E.Version /= Strings.Null_Content then
                  Write ("%YAML " & E.Version.Get & Line_End & "---");
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
        with Pre => E.Kind = Events.Scalar is
      begin
         declare
            Features : constant Analysis.Scalar_Features :=
              Analysis.Features (E.Value.Get);
         begin
            if Features.Unquoted_Single_Line then
               if P.Cur_Column + Features.Single_Line_Length + 1 <=
                 Max_Line_Length then
                  if Render_Newline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Single_Line_Plain;
                  Write (':');
                  P.Levels.Top.Position :=
                    (if In_Flow then After_Implicit_Flow_Map_Key else
                        After_Implicit_Block_Map_Key);
               else
                  Write ('?');
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (In_Flow, Features);
                  P.Levels.Top.Position :=
                    (if In_Flow then After_Explicit_Flow_Map_Key else
                        After_Explicit_Block_Map_Key);
               end if;
            elsif P.Cur_Column + Features.Single_Line_Length + 3 <=
              Max_Line_Length then
               if Render_Newline_Properties (E.Scalar_Properties) then null; end if;
               Render_Single_Line_Double_Quoted;
               Write (':');
               P.Levels.Top.Position :=
                 (if In_Flow then After_Implicit_Flow_Map_Key else
                     After_Implicit_Block_Map_Key);
            else
               Write ('?');
               if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
               Render_Scalar (In_Flow, Features);
               P.Levels.Top.Position :=
                 (if In_Flow then After_Explicit_Flow_Map_Key else
                     After_Explicit_Block_Map_Key);
            end if;
         end;
      end Render_Scalar_Mapping_Key;

      procedure Start_Block_Key_Value_Pair is
      begin
         case E.Kind is
            when Events.Scalar =>
               Next_Line;
               Render_Scalar_Mapping_Key (False);
            when Events.Mapping_Start =>
               P.Levels.Top.Position := After_Explicit_Block_Map_Key;
               Next_Line;
               Write ('?');
               Start_Node (True, No_Compact, Mapping_Start_Descriptor);
            when Events.Sequence_Start =>
               P.Levels.Top.Position := After_Implicit_Block_Map_Key;
               Next_Line;
               Write ('?');
               Start_Node (True, No_Compact, Sequence_Start_Descriptor);
            when Events.Alias =>
               if P.Cur_Column + E.Target.Get.Data'Length + 3 <=
                 Max_Line_Length then
                  Render_Alias (False);
                  Write (" :");
                  P.Levels.Top.Position := After_Implicit_Block_Map_Key;
               else
                  Write ("?");
                  Render_Alias (True);
                  P.Levels.Top.Position := After_Explicit_Block_Map_Key;
               end if;
            when Events.Mapping_End =>
               --  empty mapping must be flow-style
               Write (" {}");
               P.Levels.Pop;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected mapping key): " & E.Kind'Img;
         end case;
      end Start_Block_Key_Value_Pair;

      procedure Start_Flow_Key_Value_Pair is
      begin
         case E.Kind is
            when Events.Scalar => Render_Scalar_Mapping_Key (True);
            when Events.Mapping_Start =>
               Next_Line;
               Write ('?');
               P.Levels.Top.Position := After_Explicit_Flow_Map_Key;
               Start_Flow_Node (True, Mapping_Start_Descriptor);
            when Events.Sequence_Start =>
               Next_Line;
               Write ('?');
               P.Levels.Top.Position := After_Explicit_Flow_Map_Key;
               Start_Flow_Node (True, Sequence_Start_Descriptor);
            when Events.Alias =>
               if P.Cur_Column + E.Target.Get.Data'Length + 3 <=
                 Max_Line_Length then
                  Render_Alias (False);
                  Write (" :");
                  P.Levels.Top.Position := After_Implicit_Flow_Map_Key;
               else
                  Write ('?');
                  Render_Alias (True);
                  P.Levels.Top.Position := After_Explicit_Flow_Map_Key;
               end if;
            when Events.Mapping_End =>
               Write ('}');
               P.Levels.Pop;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected mapping key): " & E.Kind'Img;
         end case;
      end Start_Flow_Key_Value_Pair;

      procedure Start_Block_Sequence_Item is
      begin
         case E.Kind is
            when Events.Scalar =>
               Next_Line;
               Write ('-');
               if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
               Render_Scalar (False, Analysis.Features (E.Value.Get));
               P.Levels.Top.Position := After_Block_Seq_Item;
            when Events.Mapping_Start =>
               Next_Line;
               Write ('-');
               P.Levels.Top.Position := After_Block_Seq_Item;
               Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
            when Events.Sequence_Start =>
               Next_Line;
               Write ('-');
               P.Levels.Top.Position := After_Block_Seq_Item;
               Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
            when Events.Alias =>
               Next_Line;
               Write ('-');
               Render_Alias (True);
               P.Levels.Top.Position := After_Block_Seq_Item;
            when Events.Sequence_End =>
               Write (" []");
               P.Levels.Pop;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected sequence item): " & E.Kind'Img;
         end case;
      end Start_Block_Sequence_Item;

      procedure Start_Flow_Sequence_Item (After_Comma : Boolean) is
      begin
         case E.Kind is
            when Events.Scalar =>
               if After_Comma then
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
               else
                  if Render_Newline_Properties (E.Scalar_Properties) then null; end if;
               end if;
               declare
                  Features : constant Analysis.Scalar_Features :=
                    Analysis.Features (E.Value.Get);
               begin
                  if Features.Unquoted_Single_Line then
                     if P.Cur_Column + Features.Single_Line_Length + 2 <=
                       Max_Line_Length then
                        if After_Comma then
                           Write (' ');
                        end if;
                        Render_Single_Line_Plain;
                     else
                        Render_Scalar (True, Features);
                     end if;
                  elsif P.Cur_Column + Features.Single_Line_Length + 4 <=
                    Max_Line_Length then
                     if After_Comma then
                        Write (' ');
                     end if;
                     Render_Single_Line_Double_Quoted;
                  else
                     Render_Scalar (True, Features);
                  end if;
               end;
               P.Levels.Top.Position := After_Flow_Seq_Item;
            when Events.Mapping_Start =>
               Next_Line;
               P.Levels.Top.Position := After_Flow_Seq_Item;
               Start_Flow_Node (False, Mapping_Start_Descriptor);
            when Events.Sequence_Start =>
               Next_Line;
               P.Levels.Top.Position := After_Flow_Seq_Item;
               Start_Flow_Node (False, Sequence_Start_Descriptor);
            when Events.Alias =>
               Render_Alias (After_Comma);
               P.Levels.Top.Position := After_Flow_Seq_Item;
            when Events.Sequence_End =>
               Write (']');
               P.Levels.Pop;
            when others =>
               raise Presenter_Error with
                 "Unexpected event (expected sequence item): " & E.Kind'Img;
         end case;
      end Start_Flow_Sequence_Item;

   begin
      case P.Levels.Top.Position is
         when Before_Stream_Start =>
            if E.Kind /= Events.Stream_Start then
               raise Presenter_Error with "missing Stream_Start event";
            end if;
            P.Levels.Top.Position := Before_Doc_Start;
         when After_Stream_End =>
            raise Presenter_Error with "Unexpected event after stream end: " &
              E.Kind'Img;
         when Before_Doc_Start => Start_Document;
         when After_Implicit_Doc_Start =>
            P.Levels.Top.Position := Before_Doc_End;
            case E.Kind is
               when Events.Scalar =>
                  --  scalars at root level *must* have `---`
                  Write ("---");
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Long_Scalar (False, Analysis.Features (E.Value.Get));
               when Events.Mapping_Start =>
                  if E.Collection_Style /= Events.Flow and then
                    (not Events.Is_Empty (E.Collection_Properties)) then
                     Write ("---");
                     Start_Node (True, No_Compact, Mapping_Start_Descriptor);
                  else
                     Start_Node (False, All_Of_Them, Mapping_Start_Descriptor);
                  end if;
               when Events.Sequence_Start =>
                  if E.Collection_Style /= Events.Flow and then
                    (not Events.Is_Empty (E.Collection_Properties)) then
                     Write ("---");
                     Start_Node (True, No_Compact, Sequence_Start_Descriptor);
                  else
                     Start_Node (False, All_Of_Them, Sequence_Start_Descriptor);
                  end if;
               when Events.Alias =>
                  Next_Line;
                  Render_Alias (False);
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected node start): " & E.Kind'Img;
            end case;
         when After_Directives_End =>
            P.Levels.Top.Position := Before_Doc_End;
            case E.Kind is
               when Events.Scalar =>
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Long_Scalar (False, Analysis.Features (E.Value.Get));
               when Events.Mapping_Start =>
                  Start_Node (True, No_Compact, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                     Start_Node (True, No_Compact, Sequence_Start_Descriptor);
               when Events.Alias =>
                  Next_Line;
                  Render_Alias (False);
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected node start): " & E.Kind'Img;
            end case;
         when Before_Doc_End =>
            if E.Kind /= Events.Document_End then
               raise Presenter_Error with
                 "Unexpected event (expected document end): " & E.Kind'Img;
            end if;
            if E.Implicit_End then
               Write (Character'Val (10));
               P.Levels.Top.Position := After_Implicit_Doc_End;
            else
               Write (Character'Val (10) & "..." & Character'Val (10));
               P.Levels.Top.Position := Before_Doc_Start;
            end if;
         when After_Implicit_Doc_End =>
            case E.Kind is
               when Events.Document_Start =>
                  if E.Version /= Strings.Null_Content then
                     Write ("..." & Character'Val (10));
                  end if;
                  Start_Document;
               when Events.Stream_End =>
                  P.Levels.Top.Position := After_Stream_End;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected document start or stream end): " &
                    E.Kind'Img;
            end case;
         when After_Implicit_Map_Start =>
            case E.Kind is
               when Events.Scalar =>
                  Render_Scalar_Mapping_Key (False);
               when Events.Mapping_Start =>
                  P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  Write ('?');
                  Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                  P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  Write ('?');
                  Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
               when Events.Alias =>
                  if P.Cur_Column + E.Target.Get.Data'Length + 3 <=
                    Max_Line_Length then
                     Render_Alias (False);
                     Write (" :");
                     P.Levels.Top.Position := After_Implicit_Block_Map_Key;
                  else
                     Write ('?');
                     Render_Alias (True);
                     P.Levels.Top.Position := After_Explicit_Block_Map_Key;
                  end if;
               when Events.Mapping_End =>
                  --  empty mapping must be flow-style
                  Write ("{}");
                  P.Levels.Pop;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping key): " & E.Kind'Img;
            end case;
         when After_Map_Header => Start_Block_Key_Value_Pair;
         when After_Explicit_Block_Map_Key =>
            Next_Line;
            Write (':');
            P.Levels.Top.Position := After_Block_Map_Value;
            case E.Kind is
               when Events.Scalar =>
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (False, Analysis.Features (E.Value.Get));
               when Events.Mapping_Start =>
                  Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                  Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
               when Events.Alias => Render_Alias (True);
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Implicit_Block_Map_Key =>
            P.Levels.Top.Position := After_Block_Map_Value;
            case E.Kind is
               when Events.Scalar =>
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (False, Analysis.Features (E.Value.Get));
               when Events.Mapping_Start =>
                  Start_Node (True, No_Compact, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                  Start_Node (True, No_Compact, Sequence_Start_Descriptor);
               when Events.Alias => Render_Alias (True);
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Block_Map_Value =>
            if E.Kind = Events.Mapping_End then
               P.Levels.Pop;
            else
               Start_Block_Key_Value_Pair;
            end if;
         when After_Seq_Header => Start_Block_Sequence_Item;
         when After_Implicit_Seq_Start =>
            case E.Kind is
               when Events.Scalar =>
                  Write ('-');
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (False, Analysis.Features (E.Value.Get));
                  P.Levels.Top.Position := After_Block_Seq_Item;
               when Events.Mapping_Start =>
                  Write ('-');
                  P.Levels.Top.Position := After_Block_Seq_Item;
                  Start_Node (True, All_Of_Them, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                  Write ('-');
                  P.Levels.Top.Position := After_Block_Seq_Item;
                  Start_Node (True, All_Of_Them, Sequence_Start_Descriptor);
               when Events.Alias =>
                  Write ('-');
                  Render_Alias (True);
                  P.Levels.Top.Position := After_Block_Seq_Item;
               when Events.Sequence_End =>
                  Write ("[]");
                  P.Levels.Pop;
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected sequence item): " & E.Kind'Img;
            end case;
         when After_Block_Seq_Item =>
            if E.Kind = Events.Sequence_End then
               P.Levels.Pop;
            else
               Start_Block_Sequence_Item;
            end if;
         when After_Flow_Map_Start => Start_Flow_Key_Value_Pair;
         when After_Implicit_Flow_Map_Key =>
            P.Levels.Top.Position := After_Flow_Map_Value;
            case E.Kind is
               when Events.Scalar =>
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (True, Analysis.Features (E.Value.Get));
               when Events.Mapping_Start =>
                  Start_Flow_Node (True, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                  Start_Flow_Node (True, Sequence_Start_Descriptor);
               when Events.Alias => Render_Alias (True);
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Explicit_Flow_Map_Key =>
            Next_Line;
            Write (':');
            P.Levels.Top.Position := After_Flow_Map_Value;
            case E.Kind is
               when Events.Scalar =>
                  if Render_Inline_Properties (E.Scalar_Properties) then null; end if;
                  Render_Scalar (True, Analysis.Features (E.Value.Get));
               when Events.Mapping_Start =>
                  Start_Flow_Node (True, Mapping_Start_Descriptor);
               when Events.Sequence_Start =>
                  Start_Flow_Node (True, Sequence_Start_Descriptor);
               when Events.Alias => Render_Alias (True);
               when others =>
                  raise Presenter_Error with
                    "Unexpected event (expected mapping value): " & E.Kind'Img;
            end case;
         when After_Flow_Map_Value =>
            if E.Kind = Events.Mapping_End then
               Write ('}');
               P.Levels.Pop;
            else
               Write (',');
               Start_Flow_Key_Value_Pair;
            end if;
         when After_Flow_Seq_Start => Start_Flow_Sequence_Item (False);
         when After_Flow_Seq_Item =>
            if E.Kind = Events.Sequence_End then
               Write (']');
               P.Levels.Pop;
            else
               Write (',');
               Start_Flow_Sequence_Item (True);
            end if;
      end case;
   end Put;

   procedure Put (P : in out Presenter;
                  S : in out Streams.Event_Stream'Class) is
      Cur : Events.Event := S.Next;
   begin
      loop
         P.Put (Cur);
         exit when Cur.Kind = Events.Stream_End;
         Cur := S.Next;
      end loop;
   end Put;

end Yaml.Presenting;
