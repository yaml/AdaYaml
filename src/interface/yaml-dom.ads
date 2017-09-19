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

   No_Document : constant Document_Reference;
   No_Node     : constant Optional_Node_Reference;

   -----------------------------------------------------------------------------
   --                      constructors and comparators                       --
   -----------------------------------------------------------------------------

   --  uses the given pool for all text content
   function New_Document (Pool : Text.Pool.Reference :=
                            Text.Pool.With_Capacity (Text.Pool.Default_Size))
                          return Document_Reference;

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

   function "=" (Left, Right : Document_Reference) return Boolean;

   --  checks whether the content of two nodes is identical
   function "=" (Left, Right : Node_Reference) return Boolean;

   --  checks whether the two references reference the same node
   function Same_Node (Left, Right : Node_Reference) return Boolean;

   -----------------------------------------------------------------------------
   --                              data access                                --
   -----------------------------------------------------------------------------

   function Is_Empty (Object : Document_Reference) return Boolean;
   function Root (Object : Document_Reference'Class) return Node_Reference
     with Pre => not Is_Empty (Object);
   procedure Set_Root (Object : in out Document_Reference;
                       Value : Node_Reference'Class);
   procedure Set_Root (Object : in out Document_Reference;
                       Value : Optional_Node_Reference'Class);

   function Value (Object : Node_Reference) return Accessor;
   function Value (Object : Optional_Node_Reference) return Accessor;

   function Required (Object : Optional_Node_Reference'Class) return Node_Reference;
   function Optional (Object : Node_Reference'Class) return Optional_Node_Reference;

private
   type Node_Pointer is not null access all Node.Instance;

   function Nodes_Equal (Left, Right : access Node.Instance) return Boolean;

   type Document_Instance is new Refcount_Base with record
      Root_Node : access Node.Instance;
      Pool      : Text.Pool.Reference;
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

   No_Document : constant Document_Reference :=
     (Ada.Finalization.Controlled with Data =>
         new Document_Instance'(Refcount_Base with
        Root_Node =>  null, Pool => <>));

   No_Node : constant Optional_Node_Reference :=
     (Ada.Finalization.Controlled with Data => null, Document => null);
end Yaml.Dom;
