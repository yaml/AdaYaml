--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers;
with Text.Pool;
with Yaml.Tags;

limited with Yaml.Dom.Node;

package Yaml.Dom is
   type Document_Reference is tagged private;

   type Node_Kind is (Scalar, Sequence, Mapping);

   type Node_Reference is tagged private;
   type Optional_Node_Reference is tagged private;
   type Accessor (Data : not null access Node.Instance) is limited private
     with Implicit_Dereference => Data;

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
   function "=" (Left, Right : Node_Reference) return Boolean;

   --  checks whether the two references reference the same node
   function Same_Node (Left, Right : Node_Reference) return Boolean;

   -----------------------------------------------------------------------------
   --                              data access                                --
   -----------------------------------------------------------------------------

   function Value (Object : Node_Reference) return Accessor;
   function Value (Object : Optional_Node_Reference) return Accessor;

   function Required (Object : Optional_Node_Reference'Class) return Node_Reference;
   function Optional (Object : Node_Reference'Class) return Optional_Node_Reference;

   Null_Reference : constant Optional_Node_Reference;
private
   type Node_Pointer is not null access all Node.Instance;

   function "=" (Left, Right : Node_Pointer) return Boolean;

   type Document_Instance is new Refcount_Base with record
      Root : Node_Pointer;
      Pool : Text.Pool.Reference;
   end record;

   type Document_Reference is new Ada.Finalization.Controlled with record
      Data : not null access Document_Instance;
   end record;

   overriding procedure Adjust (Object : in out Document_Reference);
   overriding procedure Finalize (Object : in out Document_Reference);

   type Node_Reference is new Ada.Finalization.Controlled with record
      Data : Node_Pointer;
      Document : not null access Document_Instance;
   end record;

   overriding procedure Adjust (Object : in out Node_Reference);
   overriding procedure Finalize (Object : in out Node_Reference);

   type Optional_Node_Reference is new Ada.Finalization.Controlled with record
      Data : access Node.Instance;
      Document : access Document_Instance;
   end record;

   overriding procedure Adjust (Object : in out Optional_Node_Reference);
   overriding procedure Finalize (Object : in out Optional_Node_Reference);

   type Accessor (Data : not null access Node.Instance) is limited null record;

   Null_Reference : constant Optional_Node_Reference :=
     (Ada.Finalization.Controlled with Data => null, Document => null);
end Yaml.Dom;
