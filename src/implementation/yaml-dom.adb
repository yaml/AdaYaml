--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Hashed_Sets;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

package body Yaml.Dom is
   use type Text.Reference;
   use type Count_Type;

   type Sequence_Iterator is new Sequence_Iterators.Forward_Iterator with record
      Container : not null access constant Node_Sequence;
   end record;

   type Mapping_Iterator is new Mapping_Iterators.Forward_Iterator with record
      Container : not null access constant Node_Mapping;
   end record;

   function First (Object : Sequence_Iterator) return Sequence_Cursor is
     ((Container => Object.Container, Index => 1));

   function Next (Object : Sequence_Iterator; Position : Sequence_Cursor)
                  return Sequence_Cursor is
     ((Container => Object.Container,
       Index => Count_Type'Min (Position.Index + 1, Object.Container.Data.Length + 1)));

   function First (Object : Mapping_Iterator) return Mapping_Cursor is
     ((Container => Object.Container, Index => 1));

   function Next (Object : Mapping_Iterator; Position : Mapping_Cursor)
                  return Mapping_Cursor is
     ((Container => Object.Container,
       Index => Count_Type'Min (Position.Index + 1, Object.Container.Data.Length + 1)));

   function Hash (Value : System.Address) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (System.Storage_Elements.To_Integer (Value)));

   package Address_Sets is new Ada.Containers.Hashed_Sets
     (System.Address, Hash, System."=", System."=");

   type Nullable_Node_Pointer is access all Node_Instance;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Instance, Nullable_Node_Pointer);

   procedure Decrease_Refcount (Object : not null access Document_Instance) is
   begin
      if Object.Refcount = 1 then
         declare
            Visited : Address_Sets.Set;

            procedure Visit (Node : not null access Node_Instance) is
               Ptr : Nullable_Node_Pointer := Nullable_Node_Pointer (Node);
            begin
               if Visited.Contains (Node.all'Address) then
                  return;
               end if;
               Visited.Include (Node.all'Address);
               case Node.Kind is
                  when Scalar => null;
                  when Sequence =>
                     for Item of Node.Items.Data loop
                        Visit (Item);
                     end loop;
                  when Mapping =>
                     for Raw of Node.Pairs.Data loop
                        Visit (Raw.Key);
                        Visit (Raw.Value);
                     end loop;
               end case;
               Free (Ptr);
            end Visit;
         begin
            Visit (Object.Root);
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
      Increase_Refcount (Object.Document);
   end Adjust;

   procedure Finalize (Object : in out Optional_Node_Reference) is
   begin
      Dom.Decrease_Refcount (Object.Document);
   end Finalize;

   function New_Scalar (Tag : Text.Reference;
                        Content : Text.Reference := Text.Empty)
                        return not null access Node_Instance is
      (new Node_Instance'(Kind => Scalar, Tag => Tag, Content => Content));

   function New_Sequence (Document : not null access Document_Instance;
                          Tag : Text.Reference)
                          return not null access Node_Instance is
     (new Node_Instance'(Kind => Sequence, Tag => Tag, Items =>
                             (Document => Document, Data => <>)));

   function New_Mapping (Document : not null access Document_Instance;
                         Tag : Text.Reference)
                         return not null access Node_Instance is
     (new Node_Instance'(Kind => Mapping, Tag => Tag, Pairs =>
                             (Document => Document, Data => <>)));

   function New_Document (Root_Kind : Node_Kind) return Document_Reference is
      Pool : Text.Pool.Reference;
   begin
      Pool.Create (8092);
      return New_Document (Root_Kind, Pool);
   end New_Document;

   function New_Document (Root_Kind : Node_Kind;
                          Pool : Text.Pool.Reference) return Document_Reference
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Storage_Elements.Integer_Address, Node_Pointer);
      Dummy : constant not null access Node_Instance := Convert (1);
   begin
      return Ret : constant Document_Reference :=
        (Ada.Finalization.Controlled with Data =>
            new Document_Instance'(Ada.Finalization.Limited_Controlled with
               Pool => Pool, Root => Dummy, Refcount => 1)) do
         case Root_Kind is
         when Scalar =>
            Ret.Data.Root := New_Scalar (Yaml.Tags.Question_Mark);
            when Sequence =>
               Ret.Data.Root := New_Sequence (Ret.Data, Yaml.Tags.Question_Mark);
            when Mapping =>
               Ret.Data.Root := New_Mapping (Ret.Data, Yaml.Tags.Question_Mark);
         end case;

      end return;
   end New_Document;


   function New_Scalar (Parent : Document_Reference'Class;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Content : String := "") return Node_Reference is
     ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
                 new Node_Instance'(Tag => Tag, Kind => Scalar,
                                    Content => Parent.Data.Pool.From_String (Content))));

   function New_Scalar (Parent : Document_Reference'Class;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Content : Text.Reference) return Node_Reference is
     ((Ada.Finalization.Controlled with Document => Parent.Data, Data =>
            new Node_Instance'(Tag => Tag, Kind => Scalar, Content => Content)));

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

   function "=" (Left, Right : Node_Instance) return Boolean is
     (Left.Kind = Right.Kind and then
      Left.Tag = Right.Tag and then
        (case Left.Kind is
            when Scalar => Left.Content = Right.Content,
            when Sequence => Left.Items = Right.Items,
            when Mapping => Left.Pairs = Right.Pairs));

   function "=" (Left, Right : Node_Reference) return Boolean is
     (Same_Node (Left, Right) or else Left.Data.all = Right.Data.all);

   --  checks whether the two references reference the same node
   function Same_Node (Left, Right : Node_Reference) return Boolean is
     (Left.Data = Right.Data);

   function "=" (Left, Right : Node_Sequence) return Boolean is
   begin
      if Left.Data.Length /= Right.Data.Length then
         return False;
      else
         for Index in 1 .. Left.Data.Length loop
            if Left.Data (Positive (Index)) /= Right.Data (Positive (Index)) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   function "=" (Left, Right : Node_Mapping) return Boolean is
   begin
      if Left.Data.Length /= Right.Data.Length then
         return False;
      else
         for Left_Pair of Left loop
            for Right_Pair of Right loop
               if Left_Pair.Key = Right_Pair.Key then
                  if Left_Pair.Value = Right_Pair.Value then
                     goto Found_Match;
                  else
                     return False;
                  end if;
               end if;
            end loop;
            return False;
            <<Found_Match>>
         end loop;
         return True;
      end if;
   end "=";

   function Value (Object : Node_Reference) return Accessor is
     ((Data => Object.Data));

   function Value (Object : Optional_Node_Reference) return Accessor is
     ((Data => Object.Data));

   function Required (Object : Optional_Node_Reference'Class)
                      return Node_Reference is
   begin
      Increase_Refcount (Object.Document);
      return (Ada.Finalization.Controlled with Document => Object.Document,
              Data => Object.Data);
   end Required;

   function Optional (Object : Node_Reference'Class)
                      return Optional_Node_Reference is
   begin
      Increase_Refcount (Object.Document);
      return (Ada.Finalization.Controlled with Document => Object.Document,
              Data => Object.Data);
   end Optional;

   function Element (Object : Node_Sequence'Class; Index : Positive)
                     return Node_Reference is
   begin
      Increase_Refcount (Object.Document);
      return ((Ada.Finalization.Controlled with Data => Object.Data.Element (Index),
               Document => Object.Document));
   end Element;

   function Item (Object : Node_Mapping; Index : Positive) return Pair is
      Raw : constant Raw_Pair := Object.Data.Element (Index);
   begin
      Increase_Refcount (Object.Document);
      Increase_Refcount (Object.Document);
      return (Key => (Ada.Finalization.Controlled with Data => Raw.Key,
                      Document => Object.Document),
              Value => (Ada.Finalization.Controlled with Data => Raw.Value,
                          Document => Object.Document));
   end Item;

   function Length (Object : Node_Sequence) return Count_Type is
     (Object.Data.Length);

   function Length (Object : Node_Mapping) return Count_Type is
     (Object.Data.Length);

   function Find (Object : Node_Mapping'Class; Key : String)
                  return Node_Reference is
   begin
      for Node of Object.Data loop
         if Node.Key.Kind = Scalar then
            if Node.Key.Content = Key then
               Increase_Refcount (Object.Document);
               return (Ada.Finalization.Controlled with Data => Node.Value,
                       Document => Object.Document);
            end if;
         end if;
      end loop;
      raise Constraint_Error with "Mapping contains no key """ & Key & """";
   end Find;

   function Has_Element (Position : Sequence_Cursor) return Boolean is
     (Position.Container /= null and then Position.Index > 0 and then
      Position.Index <= Position.Container.Data.Length);
   function Has_Element (Position : Mapping_Cursor) return Boolean is
     (Position.Container /= null and then Position.Index > 0 and then
      Position.Index <= Position.Container.Data.Length);

   function Iterate (Container : Node_Sequence)
                     return Sequence_Iterators.Forward_Iterator'Class is
     (Sequence_Iterator'(Container => Container'Unrestricted_Access));

   function Iterate (Container : Node_Mapping)
                     return Mapping_Iterators.Forward_Iterator'Class is
     (Mapping_Iterator'(Container => Container'Unrestricted_Access));

   function Sequence_Value (Container : Node_Sequence'Class;
                            Position : Sequence_Cursor) return Node_Reference is
     (Container.Element (Positive (Position.Index)));

   function Mapping_Value (Container : Node_Mapping'Class;
                           Position : Mapping_Cursor) return Pair is
     (Container.Item (Positive (Position.Index)));
end Yaml.Dom;
