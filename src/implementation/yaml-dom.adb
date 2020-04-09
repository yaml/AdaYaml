--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;
with Yaml.Dom.Node;
with Yaml.Dom.Sequence_Data;
with Yaml.Dom.Mapping_Data;
with Yaml.Dom.Node_Memory;

package body Yaml.Dom is
   use type Node.Instance;

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
      if Object.Refcount = 1 and then Object.Root_Node /= null then
         declare
            Memory    : Node_Memory.Instance;

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
            end Visit;

            procedure Visit_Pair (Key, Value : not null access Node.Instance) is
            begin
               Visit (Key);
               Visit (Value);
            end Visit_Pair;
         begin
            Visit (Object.Root_Node);
            while not Memory.Is_Empty loop
               declare
                  Ptr : Nullable_Node_Pointer :=
                    Nullable_Node_Pointer (Memory.Pop_First);
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

   function New_Sequence (Document : not null access Document_Instance;
                          Tag : Text.Reference;
                          Style : Collection_Style_Type)
                          return Node_Pointer is
     (new Node.Instance'(Kind => Sequence, Tag => Tag, Sequence_Style =>  Style,
                         Items => For_Document (Document)));

   function New_Mapping (Document : not null access Document_Instance;
                         Tag : Text.Reference;
                         Style : Collection_Style_Type)
                         return Node_Pointer is
     (new Node.Instance'(Kind => Mapping, Tag => Tag, Mapping_Style => Style,
                         Pairs => For_Document (Document)));

   function New_Document (Pool : Text.Pool.Reference :=
                            Text.Pool.With_Capacity (Text.Pool.Default_Size);
                          Implicit_Start, Implicit_End : Boolean := True)
                          return Document_Reference
   is
   begin
      return (Ada.Finalization.Controlled with Data =>
                 new Document_Instance'(Refcount_Base with
                  Pool => Pool, Root_Node => null,
                Implicit_Start => Implicit_Start,
                Implicit_End => Implicit_End));
   end New_Document;

   function New_Scalar (Parent : Document_Reference'Class;
                        Content : String := "";
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Style : Scalar_Style_Type := Any)
                        return Node_Reference is
   begin
      Increase_Refcount (Parent.Data);
      return ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
                  new Node.Instance'(Tag => Tag, Kind => Scalar,
                                     Scalar_Style => Style,
                                     Content => Parent.Data.Pool.From_String (Content))));
   end New_Scalar;

   function New_Scalar (Parent : Document_Reference'Class;
                        Content : Text.Reference;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Style : Scalar_Style_Type := Any)
                        return Node_Reference is
   begin
      Increase_Refcount (Parent.Data);
      return ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
                  new Node.Instance'(Tag => Tag, Kind => Scalar,
                                     Scalar_Style => Style, Content => Content)));
   end New_Scalar;

   function New_Sequence (Parent : Document_Reference'Class;
                          Tag : Text.Reference := Yaml.Tags.Question_Mark;
                          Style : Collection_Style_Type := Any)
                          return Node_Reference is
   begin
      Increase_Refcount (Parent.Data);
      return ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
                 New_Sequence (Parent.Data, Tag, Style)));
   end New_Sequence;

   function New_Mapping (Parent : Document_Reference'Class;
                         Tag : Text.Reference := Yaml.Tags.Question_Mark;
                         Style : Collection_Style_Type := Any)
                         return Node_Reference is
   begin
      Increase_Refcount (Parent.Data);
      return ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
                 New_Mapping (Parent.Data, Tag, Style)));
   end New_Mapping;

   function Nodes_Equal (Left, Right : access Node.Instance) return Boolean is
     (Left = Right or else (Left /= null and then Right /= null and then
                            Left.all = Right.all));

   function "=" (Left, Right : Document_Reference) return Boolean is
     (Nodes_Equal (Left.Data.Root_Node, Right.Data.Root_Node));

   function "=" (Left, Right : Node_Reference) return Boolean is
     (Same_Node (Left, Right) or else Left.Data.all = Right.Data.all);

   --  checks whether the two references reference the same node
   function Same_Node (Left, Right : Node_Reference) return Boolean is
     (Left.Data = Right.Data);

   function Is_Empty (Object : Document_Reference) return Boolean is
     (Object.Data.Root_Node = null);

   function Root (Object : Document_Reference'Class) return Node_Reference is
   begin
      Increase_Refcount (Object.Data);
      return (Ada.Finalization.Controlled with Document => Object.Data,
              Data => Node_Pointer (Object.Data.Root_Node));
   end Root;

   procedure Set_Root (Object : Document_Reference;
                       Value : Node_Reference'Class) is
   begin
      Object.Data.Root_Node := Value.Data;
   end Set_Root;

   procedure Set_Root (Object : Document_Reference;
                       Value : Optional_Node_Reference'Class) is
   begin
      Object.Data.Root_Node := Value.Data;
   end Set_Root;

   function Starts_Implicitly (Object : Document_Reference) return Boolean is
     (Object.Data.Implicit_Start);

   function Ends_Implicitly (Object : Document_Reference) return Boolean is
     (Object.Data.Implicit_End);

   procedure Set_Representation_Hints (Object : Document_Reference;
                                       Implicit_Start, Implicit_End : Boolean)
   is begin
      Object.Data.Implicit_Start := Implicit_Start;
      Object.Data.Implicit_End := Implicit_End;
   end Set_Representation_Hints;

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
