--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Indefinite_Hashed_Maps;
with Yaml.Dom.Node;
with Yaml.Dom.Mapping_Data;
with Yaml.Dom.Sequence_Data;
with Yaml.Parser.Stream;

package body Yaml.Dom.Loading is
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

   package body Stream_Loading is
      function Node_From (Target_Document : not null access Document_Instance;
                          Head : Event; Tail : in out Stream.Instance;
                          Anchors : in out Anchor_Maps.Map)
                          return Node_Pointer is
      begin
         case Head.Kind is
         when Scalar =>
            return Ret : constant Node_Pointer :=
              new Node.Instance'(Kind => Scalar,
                                 Tag => Head.Scalar_Properties.Tag,
                                 Scalar_Style => Head.Scalar_Style,
                                 Content => Head.Content) do
               if Head.Scalar_Properties.Anchor.Length /= 0 then
                  Anchors.Include (Head.Scalar_Properties.Anchor, Ret);
               end if;
            end return;
         when Sequence_Start =>
            return Ret : constant Node_Pointer :=
              new Node.Instance'(Kind => Sequence,
                                 Tag => Head.Collection_Properties.Tag,
                                 Sequence_Style => Head.Collection_Style,
                                 Items => For_Document (Target_Document)) do
               declare
                  New_Head : Event := Stream.Next (Tail);
               begin
                  while New_Head.Kind /= Sequence_End loop
                     Raw_Append (Ret.Items,
                                 Node_From (Target_Document, New_Head, Tail,
                                   Anchors));
                     New_Head := Stream.Next (Tail);
                  end loop;
               end;
               if Head.Collection_Properties.Anchor.Length /= 0 then
                  Anchors.Include (Head.Collection_Properties.Anchor, Ret);
               end if;
            end return;
         when Mapping_Start =>
            return Ret : constant Node_Pointer :=
              new Node.Instance'(Kind => Mapping,
                                 Tag => Head.Collection_Properties.Tag,
                                 Mapping_Style => Head.Collection_Style,
                                 Pairs => For_Document (Target_Document)) do
               declare
                  New_Head : Event := Stream.Next (Tail);
               begin
                  while New_Head.Kind /= Mapping_End loop
                     declare
                        Key : constant not null access Node.Instance :=
                          Node_From (Target_Document, New_Head, Tail, Anchors);
                        Value : constant not null access Node.Instance :=
                          Node_From (Target_Document, Stream.Next (Tail), Tail,
                                     Anchors);
                     begin
                        Raw_Insert (Ret.Pairs, Key, Value);
                     end;
                     New_Head := Stream.Next (Tail);
                  end loop;
               end;
               if Head.Collection_Properties.Anchor.Length /= 0 then
                  Anchors.Include (Head.Collection_Properties.Anchor, Ret);
               end if;
            end return;
         when Alias =>
            declare
               Pos : constant Anchor_Maps.Cursor := Anchors.Find (Head.Target);
            begin
               if Anchor_Maps.Has_Element (Pos) then
                  return Anchor_Maps.Element (Pos);
               else
                  raise Composer_Error with "Unresolvable alias: " &
                    Head.Target.Value.Data.all;
               end if;
            end;
         when Annotation_Start =>
            raise Composer_Error with "Annotations not implemented for DOM";
         when others =>
            raise Stream_Error with "Cannot start a node from event: " &
              Head.Kind'Img;
         end case;
      end Node_From;

      function Load_One (Input : in out Stream.Instance;
                         Pool  : Text.Pool.Reference :=
                           Text.Pool.With_Capacity (Text.Pool.Default_Size))
                             return Document_Reference is
         Head : Event := Stream.Next (Input);
         Anchors : Anchor_Maps.Map;
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
            Ret.Data.Root_Node :=
              Node_From (Ret.Data, Stream.Next (Input), Input, Anchors);
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
         Anchors : Anchor_Maps.Map;
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
                  Doc.Data.Root_Node :=
                    Node_From (Doc.Data, Stream.Next (Input), Input, Anchors);
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
               Anchors.Clear;
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
