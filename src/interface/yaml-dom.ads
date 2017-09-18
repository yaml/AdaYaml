--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Indefinite_Vectors;
with Text.Pool;
with Yaml.Tags;

package Yaml.Dom is
   type Document_Reference is tagged private;

   type Node_Kind is (Scalar, Sequence, Mapping);
   type Node_Sequence is tagged limited private
     with Constant_Indexing => Element;
   type Node_Mapping is tagged limited private with Constant_Indexing => Find;

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

   --  type Sequence_Cursor is private;
   --  type Mapping_Cursor is private;

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

   function Length (Object : Node_Sequence) return Natural;
   function Length (Object : Node_Mapping) return Natural;

   --  convenience method for retrieving values of keys that are scalars.
   --  the key value is given as String. complexity is O(n) since the
   --  implementor was too lazy to use hashed maps.
   function Find (Object : Node_Mapping'Class; Key : String) return Node_Reference;

   -----------------------------------------------------------------------------
   --                           data modification                             --
   -----------------------------------------------------------------------------


   Null_Reference : constant Optional_Node_Reference;
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

   Null_Reference : constant Optional_Node_Reference :=
     (Ada.Finalization.Controlled with Data => null, Document => null);

end Yaml.Dom;
