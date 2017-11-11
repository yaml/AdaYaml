--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Yaml.Dom.Node;
with Yaml.Dom.Mapping_Data;
with Yaml.Dom.Sequence_Data;
with Yaml.Parser.Stream;

package body Yaml.Dom.Loading is
   use type Ada.Containers.Count_Type;

   package Anchor_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Text.Reference, Node_Pointer, Text.Hash, Text."=");

   procedure Raw_Append (Container : in out Sequence_Data.Instance;
                         New_Item  : not null access Node.Instance)
     with Import, Convention => Ada,
     Link_Name => "AdaYaml__Sequence_Data__Raw_Append";

   procedure Raw_Insert (Container  : in out Mapping_Data.Instance;
                         Key, Value : not null access Node.Instance)
        with Import, Convention => Ada,
     Link_Name => "AdaYaml__Mapping_Data__Raw_Insert";

   function For_Document (Document : not null access Document_Instance)
                          return Sequence_Data.Instance with Import,
     Convention => Ada, Link_Name => "AdaYaml__Sequence_Data__For_Document";

   function For_Document (Document : not null access Document_Instance)
                          return Mapping_Data.Instance with Import,
     Convention => Ada, Link_Name => "AdaYaml__Mapping_Data__For_Document";

   type Level_Type is record
      Cur, Key : access Node.Instance;
   end record;

   package Level_Vectors is new Ada.Containers.Vectors (Positive, Level_Type);

   package body Stream_Loading is
      procedure Read_Document_Content
        (Target : not null access Document_Instance;
         Input : in out Stream.Instance) is
         Anchors : Anchor_Maps.Map;

         function Start_Node (E : Event; Is_Finished : out Boolean)
                              return Node_Pointer is
         begin
            case E.Kind is
               when Scalar =>
                  Is_Finished := True;
                  return Ret : constant Node_Pointer :=
                    new Node.Instance'(Kind => Scalar,
                                       Tag => E.Scalar_Properties.Tag,
                                       Scalar_Style => E.Scalar_Style,
                                       Content => E.Content) do
                     if E.Scalar_Properties.Anchor.Length /= 0 then
                        Anchors.Include (E.Scalar_Properties.Anchor, Ret);
                     end if;
                  end return;
               when Sequence_Start =>
                  Is_Finished := False;
                  return Ret : constant Node_Pointer :=
                    new Node.Instance'(Kind => Sequence,
                                       Tag => E.Collection_Properties.Tag,
                                       Sequence_Style => E.Collection_Style,
                                       Items => For_Document (Target)) do
                     if E.Collection_Properties.Anchor.Length /= 0 then
                        Anchors.Include (E.Collection_Properties.Anchor, Ret);
                     end if;
                  end return;
               when Mapping_Start =>
                  Is_Finished := False;
                  return Ret : constant Node_Pointer :=
                    new Node.Instance'(Kind => Mapping,
                                       Tag => E.Collection_Properties.Tag,
                                       Mapping_Style => E.Collection_Style,
                                       Pairs => For_Document (Target)) do
                     if E.Collection_Properties.Anchor.Length /= 0 then
                        Anchors.Include (E.Collection_Properties.Anchor, Ret);
                     end if;
                  end return;
               when Alias =>
                  declare
                     Pos : constant Anchor_Maps.Cursor :=
                       Anchors.Find (E.Target);
                  begin
                     if Anchor_Maps.Has_Element (Pos) then
                        Is_Finished := True;
                        return Anchor_Maps.Element (Pos);
                     else
                        raise Composer_Error with "Unresolvable alias: " &
                          E.Target.Value.Data.all;
                     end if;
                  end;
               when Annotation_Start =>
                  raise Composer_Error with
                    "Annotations not implemented for DOM";
               when others =>
                  raise Stream_Error with "Cannot start a node from event: " &
                    E.Kind'Img;
            end case;
         end Start_Node;

         Is_Finished : Boolean;
         Level : Level_Type := (others => null);
         Context : Level_Vectors.Vector;
      begin
         Level.Cur := Start_Node (Stream.Next (Input), Is_Finished);
         loop
            if Is_Finished then
               exit when Context.Length = 0;
               declare
                  Parent : Level_Type := Context.Last_Element;
               begin
                  case Parent.Cur.Kind is
                     when Sequence =>
                        Raw_Append (Parent.Cur.Items, Level.Cur);
                     when Mapping =>
                        if Parent.Key = null then
                           Parent.Key := Level.Cur.all'Unchecked_Access;
                        else
                           begin
                              Raw_Insert (Parent.Cur.Pairs, Parent.Key,
                                          Level.Cur);
                           exception
                              when Constraint_Error =>
                                 raise Composer_Error with
                                   "Duplicate key in mapping";
                           end;
                           Parent.Key := null;
                        end if;
                     when Scalar =>
                        raise Program_Error with
                          "Internal error: scalar node in DOM context";
                  end case;
                  Level := Parent;
                  Context.Delete_Last;
               end;
            end if;
            declare
               E : constant Event := Stream.Next (Input);
            begin
               case E.Kind is
                  when Scalar | Sequence_Start | Mapping_Start | Alias |
                       Annotation_Start =>
                     Context.Append (Level);
                     Level := (Cur => Start_Node (E, Is_Finished), Key => null);
                  when Mapping_End =>
                     if Level.Cur.Kind /= Mapping then
                        raise Composer_Error with
                          "Unexpected mapping end (expected sequence end)";
                     end if;
                     if Level.Key /= null then
                        raise Composer_Error with
                          "Missing value for key in mapping";
                     end if;
                     Is_Finished := True;
                  when Sequence_End =>
                     if Level.Cur.Kind /= Sequence then
                        raise Composer_Error with
                          "Unexpected sequence end (expected mapping end)";
                     end if;
                     Is_Finished := True;
                  when Annotation_End =>
                     -- TODO
                     raise Composer_Error with
                       "Unexpected annotation end";
                  when Stream_Start | Document_Start | Document_End |
                       Stream_End =>
                     raise Composer_Error with
                       "Unexpected event inside document: " & E.Kind'Img;
               end case;
            end;
         end loop;
         Target.Root_Node := Level.Cur.all'Unchecked_Access;
      end Read_Document_Content;

      function Load_One (Input : in out Stream.Instance;
                         Pool  : Text.Pool.Reference :=
                           Text.Pool.With_Capacity (Text.Pool.Default_Size))
                             return Document_Reference is
         Head : Event := Stream.Next (Input);
      begin
         if Head.Kind /= Stream_Start then
            raise Stream_Error with "Unexpected event (expected stream start): " &
              Head.Kind'Img;
         end if;
         Head := Stream.Next (Input);
         if Head.Kind /= Document_Start then
            raise Stream_Error with
              "Unexpected event (expected document start): " & Head.Kind'Img;
         end if;
         return Ret : constant Document_Reference :=
           (Ada.Finalization.Controlled with
              Data => new Document_Instance'(Refcount_Base with
                    Root_Node => null, Pool => Pool,
                Implicit_Start => Head.Implicit_Start, Implicit_End => <>)) do
            Read_Document_Content (Ret.Data, Input);
            Head := Stream.Next (Input);
            if Head.Kind /= Document_End then
               raise Stream_Error with
                 "Unexpected event (expected document end): " & Head.Kind'Img;
            end if;
            Ret.Data.Implicit_End := Head.Implicit_End;
            Head := Stream.Next (Input);
            case Head.Kind is
            when Stream_End => null;
            when Document_Start =>
               raise Composer_Error with "Unexpected second document in stream";
            when others =>
               raise Stream_Error with
                 "Unexpected event (expected stream end): " & Head.Kind'Img;
            end case;
         end return;
      end Load_One;

      function Load_All (Input : in out Stream.Instance;
                         Pool  : Text.Pool.Reference :=
                           Text.Pool.With_Capacity (Text.Pool.Default_Size))
                         return Vectors.Vector is
         Head : Event := Stream.Next (Input);
      begin
         if Head.Kind /= Stream_Start then
            raise Stream_Error with "Unexpected event (expected stream start): " &
              Head.Kind'Img;
         end if;
         Head := Stream.Next (Input);
         if Head.Kind /= Document_Start then
            raise Stream_Error with
              "Unexpected event (expected document start): " & Head.Kind'Img;
         end if;
         return Ret : Vectors.Vector do
            loop
               declare
                  Doc : constant Document_Reference :=
                    (Document_Reference'(Ada.Finalization.Controlled with
                     Data => new Document_Instance'(Refcount_Base with
                       Root_Node => null, Pool => Pool,
                       Implicit_Start => Head.Implicit_Start,
                       Implicit_End => <>)));
               begin
                  Read_Document_Content (Doc.Data, Input);
                  Head := Stream.Next (Input);
                  if Head.Kind /= Document_End then
                     raise Stream_Error with
                       "Unexpected event (expected document end): " & Head.Kind'Img;
                  end if;
                  Doc.Data.Implicit_End := Head.Implicit_End;
                  Ret.Append (Doc);
               end;
               Head := Stream.Next (Input);
               exit when Head.Kind /= Document_Start;
            end loop;

            if Head.Kind /= Stream_End then
               raise Stream_Error with
                 "Unexpected event (expected stream end): " & Head.Kind'Img;
            end if;
         end return;
      end Load_All;
   end Stream_Loading;

   package Parser_Loading is new Stream_Loading (Parser.Stream);

   function From_Source (Input : Source.Pointer) return Document_Reference is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_One (P, P.Pool);
   end From_Source;

   function From_Source (Input : Source.Pointer)
                         return Vectors.Vector is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_All (P, P.Pool);
   end From_Source;

   function From_String (Input : String) return Document_Reference is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_One (P, P.Pool);
   end From_String;

   function From_String (Input : String) return Vectors.Vector is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_All (P, P.Pool);
   end From_String;
end Yaml.Dom.Loading;
