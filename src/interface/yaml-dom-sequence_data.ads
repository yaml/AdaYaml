--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Indefinite_Vectors;
with Ada.Iterator_Interfaces;

package Yaml.Dom.Sequence_Data is
   type Instance is tagged limited private
     with Default_Iterator => Iterate,
     Iterator_Element => Node_Reference,
     Constant_Indexing => Element;

   type Cursor is private;

   function "=" (Left, Right : Instance) return Boolean;

   function Capacity (Container : Instance) return Count_Type;

   procedure Reserve_Capacity (Container : in out Instance;
                               Capacity  : in     Count_Type);

   function Length (Object : Instance) return Count_Type;

   function Is_Empty (Container : Instance) return Boolean;

   procedure Clear (Container : in out Instance);

   function To_Cursor (Container : Instance;
                       Index     : Natural) return Cursor;

   function To_Index (Position  : Cursor) return Natural;

   function Has_Element (Position : Cursor) return Boolean;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   procedure Iterate (Object : Instance;
                      Process : not null access procedure
                        (Item : not null access Node.Instance));

   function Iterate (Object : Instance)
                     return Iterators.Forward_Iterator'Class;

   function Element (Object : Instance; Index : Positive) return Node_Reference;
   function Element (Object : Instance; Position : Cursor) return Node_Reference
     with Pre => Has_Element (Position);

   procedure Replace_Element (Container : in out Instance;
                              Index     : in     Positive;
                              New_Item  : in     Node_Reference);

   procedure Replace_Element (Container : in out Instance;
                              Position  : in     Cursor;
                              New_Item  : in     Node_Reference);

   procedure Insert (Container : in out Instance;
                     Before    : in     Positive;
                     New_Item  : in     Node_Reference);

   procedure Insert (Container : in out Instance;
                     Before    : in     Cursor;
                     New_Item  : in     Node_Reference);

   procedure Insert (Container : in out Instance;
                     Before    : in     Cursor;
                     New_Item  : in     Node_Reference;
                     Position  :    out Cursor);

   procedure Prepend (Container : in out Instance;
                      New_Item  : in     Node_Reference);

   procedure Append (Container : in out Instance;
                     New_Item  : in     Node_Reference);

   procedure Delete (Container : in out Instance;
                     Index     : in     Positive;
                     Count     : in     Count_Type := 1);

   procedure Delete (Container : in out Instance;
                     Position  : in out Cursor;
                     Count     : in     Count_Type := 1);

   procedure Delete_First (Container : in out Instance;
                           Count     : in     Count_Type := 1);

   procedure Delete_Last (Container : in out Instance;
                          Count     : in     Count_Type := 1);

   procedure Reverse_Elements (Container : in out Instance);

   procedure Swap (Container : in out Instance;
                   I, J      : in     Positive);

   procedure Swap (Container : in out Instance;
                   I, J      : in     Cursor);

   function First_Index (Container : Instance) return Positive;

   function First (Container : Instance) return Cursor;

   function First_Element (Container : Instance) return Node_Reference;

   function Last_Index (Container : Instance) return Natural;

   function Last (Container : Instance) return Cursor;

   function Last_Element (Container : Instance) return Node_Reference;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   No_Element : constant Cursor;
private
   package Node_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Pointer);

   type Instance is tagged limited record
      Document : not null access Document_Instance;
      Data     : Node_Vectors.Vector;
   end record;

   type Cursor is record
      Container : access constant Instance;
      Index : Natural;
   end record;

   No_Element : constant Cursor := (Container => null, Index => 0);

   package Friend_Interface is
      --  these subprograms are used by other child packages of Yaml.Dom. Since
      --  they are not memory safe, they are hidden here from the library user,
      --  but made available via importing.

      function For_Document (Document : not null access Document_Instance)
                             return Instance with Export, Convention => Ada,
        Link_Name => "AdaYaml__Sequence_Data__For_Document";

      procedure Raw_Append (Container : in out Instance;
                            New_Item  : not null access Node.Instance)
        with Export, Convention => Ada,
        Link_Name => "AdaYaml__Sequence_Data__Raw_Append";
   end Friend_Interface;
end Yaml.Dom.Sequence_Data;
