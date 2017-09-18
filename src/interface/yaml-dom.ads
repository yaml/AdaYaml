--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers;
private with Ada.Containers.Indefinite_Vectors;
with Ada.Iterator_Interfaces;
with Text.Pool;
with Yaml.Tags;

package Yaml.Dom is
   type Document_Reference is tagged private;

   type Node_Kind is (Scalar, Sequence, Mapping);
   type Node_Sequence is tagged limited private
     with Default_Iterator => Iterate,
       Iterator_Element => Node_Reference,
       Constant_Indexing => Sequence_Value;
   type Node_Mapping is tagged limited private
     with Default_Iterator => Iterate,
     Iterator_Element => Pair,
     Constant_Indexing => Mapping_Value;

   type Node_Instance (Kind : Node_Kind) is record
      Tag : Text.Reference;
      case Kind is
         when Scalar => Content : Text.Reference;
         when Sequence => Items : Node_Sequence;
         when Mapping => Pairs : Node_Mapping;
      end case;
   end record;

   type Node_Reference is tagged private;
   type Optional_Node_Reference is tagged private;
   type Accessor (Data : not null access Node_Instance) is limited private
     with Implicit_Dereference => Data;

   type Pair is record
      Key, Value : Node_Reference;
   end record;

   type Sequence_Cursor is private;
   type Mapping_Cursor is private;

   subtype Count_Type is Ada.Containers.Count_Type;

   -----------------------------------------------------------------------------
   --                      constructors and comparators                       --
   -----------------------------------------------------------------------------

   --  initializes a new Text.Pool that is used for all text content
   function New_Document (Root_Kind : Node_Kind) return Document_Reference;

   --  uses the given pool for all text content
   function New_Document (Root_Kind : Node_Kind;
                          Pool : Text.Pool.Reference) return Document_Reference;

   function New_Scalar (Parent : Document_Reference'Class;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Content : String := "") return Node_Reference;

   function New_Scalar (Parent : Document_Reference'Class;
                        Tag : Text.Reference := Yaml.Tags.Question_Mark;
                        Content : Text.Reference) return Node_Reference;

   function New_Sequence (Parent : Document_Reference'Class;
                          Tag : Text.Reference := Yaml.Tags.Question_Mark)
                          return Node_Reference;

   function New_Mapping (Parent : Document_Reference'Class;
                         Tag : Text.Reference := Yaml.Tags.Question_Mark)
                         return Node_Reference;

   --  checks whether the content of two nodes is identical
   function "=" (Left, Right : Node_Instance) return Boolean;
   function "=" (Left, Right : Node_Reference) return Boolean;

   --  checks whether the two references reference the same node
   function Same_Node (Left, Right : Node_Reference) return Boolean;

   function "=" (Left, Right : Node_Sequence) return Boolean;
   function "=" (Left, Right : Node_Mapping) return Boolean;

   -----------------------------------------------------------------------------
   --                              data access                                --
   -----------------------------------------------------------------------------

   function Value (Object : Node_Reference) return Accessor;
   function Value (Object : Optional_Node_Reference) return Accessor;

   function Required (Object : Optional_Node_Reference'Class) return Node_Reference;
   function Optional (Object : Node_Reference'Class) return Optional_Node_Reference;

   function Element (Object : Node_Sequence'Class; Index : Positive)
                     return Node_Reference;
   function Item (Object : Node_Mapping; Index : Positive) return Pair;

   function Length (Object : Node_Sequence) return Count_Type;
   function Length (Object : Node_Mapping) return Count_Type;

   --  convenience method for retrieving values of keys that are scalars.
   --  the key value is given as String. complexity is O(n) since the
   --  implementor was too lazy to use hashed maps.
   function Find (Object : Node_Mapping'Class; Key : String) return Node_Reference;

   --  subprograms used for iteration

   function Has_Element (Position : Sequence_Cursor) return Boolean;
   function Has_Element (Position : Mapping_Cursor) return Boolean;

   package Sequence_Iterators is new Ada.Iterator_Interfaces
     (Sequence_Cursor, Has_Element);
   package Mapping_Iterators is new Ada.Iterator_Interfaces
     (Mapping_Cursor, Has_Element);

   function Iterate (Container : Node_Sequence)
                     return Sequence_Iterators.Forward_Iterator'Class;
   function Iterate (Container : Node_Mapping)
                     return Mapping_Iterators.Forward_Iterator'Class;

   function Sequence_Value (Container : Node_Sequence'Class;
                            Position : Sequence_Cursor) return Node_Reference
     with Pre => Has_Element (Position);
   function Mapping_Value (Container : Node_Mapping'Class;
                           Position : Mapping_Cursor) return Pair
     with Pre => Has_Element (Position);

   -----------------------------------------------------------------------------
   --                           data modification                             --
   -----------------------------------------------------------------------------


   Null_Reference : constant Optional_Node_Reference;
   No_Sequence_Item : constant Sequence_Cursor;
   No_Mapping_Item : constant Mapping_Cursor;
private
   type Node_Pointer is not null access Node_Instance;

   package Node_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Pointer);

   type Document_Instance is new Refcount_Base with record
      Root : not null access Node_Instance;
      Pool : Text.Pool.Reference;
   end record;

   type Document_Reference is new Ada.Finalization.Controlled with record
      Data : not null access Document_Instance;
   end record;

   overriding procedure Adjust (Object : in out Document_Reference);
   overriding procedure Finalize (Object : in out Document_Reference);

   type Node_Reference is new Ada.Finalization.Controlled with record
      Data : not null access Node_Instance;
      Document : not null access Document_Instance;
   end record;

   overriding procedure Adjust (Object : in out Node_Reference);
   overriding procedure Finalize (Object : in out Node_Reference);

   type Optional_Node_Reference is new Ada.Finalization.Controlled with record
      Data : access Node_Instance;
      Document : access Document_Instance;
   end record;

   overriding procedure Adjust (Object : in out Optional_Node_Reference);
   overriding procedure Finalize (Object : in out Optional_Node_Reference);

   type Accessor (Data : not null access Node_Instance) is limited null record;

   type Node_Array is array (Positive range <>) of
     not null access Node_Instance;

   type Node_Sequence is tagged limited record
      Document : not null access Document_Instance;
      Data     : Node_Vectors.Vector;
   end record;

   type Raw_Pair is record
      Key, Value : not null access Node_Instance;
   end record;

   package Pair_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Raw_Pair);

   type Pair_Array is array (Positive range <>) of
     not null access Raw_Pair;

   type Node_Mapping is tagged limited record
      Document : not null access Document_Instance;
      Data     : Pair_Vectors.Vector;
   end record;

   type Sequence_Cursor is record
      Container : access constant Node_Sequence;
      Index : Count_Type;
   end record;

   type Mapping_Cursor is record
      Container : access constant Node_Mapping;
      Index : Count_Type;
   end record;

   Null_Reference : constant Optional_Node_Reference :=
     (Ada.Finalization.Controlled with Data => null, Document => null);

   No_Sequence_Item : constant Sequence_Cursor :=
     (Container => null, Index => 0);

   No_Mapping_Item : constant Mapping_Cursor :=
     (Container => null, Index => 0);
end Yaml.Dom;
