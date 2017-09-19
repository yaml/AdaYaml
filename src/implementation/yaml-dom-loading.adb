--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Node;
with Yaml.Dom.Mapping_Data;
with Yaml.Dom.Sequence_Data;
with Yaml.Parser.Stream;

package body Yaml.Dom.Loading is
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
                          Head : Event; Tail : in out Stream.Instance)
                          return Node_Pointer is
      begin
         case Head.Kind is
         when Scalar =>
            return new Node.Instance'(Kind => Scalar,
                                      Tag => Head.Scalar_Properties.Tag,
                                      Content => Head.Content);
         when Sequence_Start =>
            return Ret : constant Node_Pointer :=
              new Node.Instance'(Kind => Sequence,
                                 Tag => Head.Collection_Properties.Tag,
                                 Items => For_Document (Target_Document)) do
               declare
                  New_Head : Event := Stream.Next (Tail);
               begin
                  while New_Head.Kind /= Sequence_End loop
                     Raw_Append (Ret.Items,
                                 Node_From (Target_Document, New_Head, Tail));
                     New_Head := Stream.Next (Tail);
                  end loop;
               end;
            end return;
         when Mapping_Start =>
            return Ret : constant Node_Pointer :=
              new Node.Instance'(Kind => Mapping,
                                 Tag => Head.Collection_Properties.Tag,
                                 Pairs => For_Document (Target_Document)) do
               declare
                  New_Head : Event := Stream.Next (Tail);
               begin
                  while New_Head.Kind /= Mapping_End loop
                     declare
                        Key : constant not null access Node.Instance :=
                          Node_From (Target_Document, New_Head, Tail);
                        Value : constant not null access Node.Instance :=
                          Node_From (Target_Document, Stream.Next (Tail), Tail);
                     begin
                        Raw_Insert (Ret.Pairs, Key, Value);
                     end;
                     New_Head := Stream.Next (Tail);
                  end loop;
               end;
            end return;
         when others =>
            raise Stream_Error with "Cannot start a node from event: " &
              Head.Kind'Img;
         end case;
      end Node_From;

      function Load_One (Input : in out Stream.Instance)
                             return Document_Reference is
         Head : Event := Stream.Next (Input);
         Pool : Text.Pool.Reference;
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
         Pool.Create (8092);
         return Ret : constant Document_Reference :=
           (Ada.Finalization.Controlled with
              Data => new Document_Instance'(Refcount_Base with
                    Root_Node => Dummy, Pool => Pool)) do
            Ret.Data.Root_Node :=
              Node_From (Ret.Data, Stream.Next (Input), Input);
            Head := Stream.Next (Input);
            if Head.Kind /= Document_End then
               raise Stream_Error with
                 "Unexpected event (expected document end): " & Head.Kind'Img;
            end if;
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

      function Load_All (Input : in out Stream.Instance)
                         return Document_Vectors.Vector is
         Pool : Text.Pool.Reference;
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
         Pool.Create (8092);
         return Ret : Document_Vectors.Vector do
            loop
               declare
                  Doc : constant Document_Reference :=
                    (Document_Reference'(Ada.Finalization.Controlled with
                     Data => new Document_Instance'(Refcount_Base with
                       Root_Node => Dummy, Pool => Pool)));
               begin
                  Doc.Data.Root_Node :=
                    Node_From (Doc.Data, Stream.Next (Input), Input);
                  Ret.Append (Doc);
               end;
               Head := Stream.Next (Input);
               if Head.Kind /= Document_End then
                  raise Stream_Error with
                    "Unexpected event (expected document end): " & Head.Kind'Img;
               end if;
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
      return Parser_Loading.Load_One (P);
   end From_Source;

   function From_Source (Input : Source.Pointer)
                         return Document_Vectors.Vector is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_All (P);
   end From_Source;

   function From_String (Input : String) return Document_Reference is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_One (P);
   end From_String;

   function From_String (Input : String) return Document_Vectors.Vector is
      P : Yaml.Parser.Instance;
   begin
      P.Set_Input (Input);
      return Parser_Loading.Load_All (P);
   end From_String;
end Yaml.Dom.Loading;
