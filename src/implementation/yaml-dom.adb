--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Indefinite_Vectors;
with Ada.Unchecked_Deallocation;
with Yaml.Dom.Node;
with Yaml.Dom.Sequence_Data;
with Yaml.Dom.Mapping_Data;
with Yaml.Dom.Node_Memory;

package body Yaml.Dom is
   use type Text.Reference;
   use type Count_Type;
   use type Node.Instance;
   use type System.Address;

   package Node_Pointer_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Node_Pointer);

   function For_Document (Document : not null access Document_Instance)
                          return Sequence_Data.Instance with Import,
     Convention => Ada, Link_Name => "AdaYaml__Sequence_Data__For_Document";

   function For_Document (Document : not null access Document_Instance)
                          return Mapping_Data.Instance with Import,
     Convention => Ada, Link_Name => "AdaYaml__Mapping_Data__For_Document";

   type Nullable_Node_Pointer is access all Node.Instance;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node.Instance, Nullable_Node_Pointer);

   procedure Decrease_Refcount (Object : not null access Document_Instance) is
   begin
      if Object.Refcount = 1 then
         declare
            Memory    : Node_Memory.Instance;
            To_Delete : Node_Pointer_Vectors.Vector;

            procedure Visit_Pair (Key, Value : not null access Node.Instance);

            procedure Visit (Cur : not null access Node.Instance) is
               Visited : Boolean;
            begin
               Memory.Visit (Cur, Visited);
               if Visited then
                  return;
               end if;
               case Cur.Kind is
                  when Scalar => null;
                  when Sequence => Cur.Items.Iterate (Visit'Access);
                  when Mapping => Cur.Pairs.Iterate (Visit_Pair'Access);
               end case;
               To_Delete.Append (Cur);
            end Visit;

            procedure Visit_Pair (Key, Value : not null access Node.Instance) is
            begin
               Visit (Key);
               Visit (Value);
            end Visit_Pair;
         begin
            Visit (Object.Root_Node);
            for Cur of To_Delete loop
               declare
                  Ptr : Nullable_Node_Pointer := Nullable_Node_Pointer (Cur);
               begin
                  Free (Ptr);
               end;
            end loop;
         end;
      end if;
      Yaml.Decrease_Refcount (Object);
   end Decrease_Refcount;

   procedure Adjust (Object : in out Document_Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Document_Reference) is
   begin
      Dom.Decrease_Refcount (Object.Data);
   end Finalize;

   procedure Adjust (Object : in out Node_Reference) is
   begin
      Increase_Refcount (Object.Document);
   end Adjust;

   procedure Finalize (Object : in out Node_Reference) is
   begin
      Dom.Decrease_Refcount (Object.Document);
   end Finalize;

   procedure Adjust (Object : in out Optional_Node_Reference) is
   begin
      if Object.Document /= null then
         Increase_Refcount (Object.Document);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Optional_Node_Reference) is
   begin
      if Object.Document /= null then
         Dom.Decrease_Refcount (Object.Document);
      end if;
   end Finalize;

   function New_Scalar (Tag : Text.Reference;
                        Content : Text.Reference := Text.Empty)
                        return Node_Pointer is
      (new Node.Instance'(Kind => Scalar, Tag => Tag, Content => Content));

   function New_Sequence (Document : not null access Document_Instance;
                          Tag : Text.Reference)
                          return Node_Pointer is
     (new Node.Instance'(Kind => Sequence, Tag => Tag,
                         Items => For_Document (Document)));

   function New_Mapping (Document : not null access Document_Instance;
                         Tag : Text.Reference)
                         return Node_Pointer is
     (new Node.Instance'(Kind => Mapping, Tag => Tag,
                         Pairs => For_Document (Document)));

   function New_Document (Root_Kind : Node_Kind) return Document_Reference is
      Pool : Text.Pool.Reference;
   begin
      Pool.Create (8092);
      return New_Document (Root_Kind, Pool);
   end New_Document;

   function New_Document (Root_Kind : Node_Kind;
                          Pool : Text.Pool.Reference) return Document_Reference
   is
   begin
      return Ret : constant Document_Reference :=
        (Ada.Finalization.Controlled with Data =>
            new Document_Instance'(Ada.Finalization.Limited_Controlled with
               Pool => Pool, Root_Node => Dummy, Refcount => 1)) do
         case Root_Kind is
         when Scalar =>
            Ret.Data.Root_Node := New_Scalar (Yaml.Tags.Question_Mark);
            when Sequence =>
               Ret.Data.Root_Node :=
                 New_Sequence (Ret.Data, Yaml.Tags.Question_Mark);
            when Mapping =>
               Ret.Data.Root_Node :=
                 New_Mapping (Ret.Data, Yaml.Tags.Question_Mark);
         end case;

      end return;
   end New_Document;


   function New_Scalar (Parent : Document_Reference'Class;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Content : String := "") return Node_Reference is
     ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
                 new Node.Instance'(Tag => Tag, Kind => Scalar,
                                    Content => Parent.Data.Pool.From_String (Content))));

   function New_Scalar (Parent : Document_Reference'Class;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Content : Text.Reference) return Node_Reference is
     ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
            new Node.Instance'(Tag => Tag, Kind => Scalar, Content => Content)));

   function New_Sequence (Parent : Document_Reference'Class;
                          Tag : Text.Reference := Yaml.Tags.Question_Mark)
                          return Node_Reference is
     ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
            New_Sequence (Parent.Data, Tag)));

   function New_Mapping (Parent : Document_Reference'Class;
                         Tag : Text.Reference := Yaml.Tags.Question_Mark)
                         return Node_Reference is
     ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
            New_Mapping (Parent.Data, Tag)));

   function "=" (Left, Right : Node_Pointer) return Boolean is
     (Left.all'Address = Right.all'Address or else Left.all = Right.all);

   function "=" (Left, Right : Node_Reference) return Boolean is
     (Same_Node (Left, Right) or else Left.Data.all = Right.Data.all);

   --  checks whether the two references reference the same node
   function Same_Node (Left, Right : Node_Reference) return Boolean is
     (Left.Data.all'Address = Right.Data.all'Address);

   function Root (Object : Document_Reference'Class) return Node_Reference is
   begin
      Increase_Refcount (Object.Data);
      return (Ada.Finalization.Controlled with Document => Object.Data,
              Data => Object.Data.Root_Node);
   end Root;

   function Value (Object : Node_Reference) return Accessor is
     ((Data => Object.Data));

   function Value (Object : Optional_Node_Reference) return Accessor is
     ((Data => Object.Data));

   function Required (Object : Optional_Node_Reference'Class)
                      return Node_Reference is
   begin
      Increase_Refcount (Object.Document);
      return (Ada.Finalization.Controlled with Document => Object.Document,
              Data => Node_Pointer (Object.Data));
   end Required;

   function Optional (Object : Node_Reference'Class)
                      return Optional_Node_Reference is
   begin
      Increase_Refcount (Object.Document);
      return (Ada.Finalization.Controlled with Document => Object.Document,
              Data => Object.Data);
   end Optional;
end Yaml.Dom;
