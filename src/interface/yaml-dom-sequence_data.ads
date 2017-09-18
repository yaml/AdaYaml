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

   function Length (Object : Instance) return Count_Type;

   function Has_Element (Position : Cursor) return Boolean;

   function First (Object : Instance) return Cursor;

   function Next (Position : Cursor) return Cursor;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   procedure Iterate (Object : Instance;
                      Process : not null access procedure
                        (Item : not null access Node.Instance));

   function Iterate (Object : Instance)
                     return Iterators.Forward_Iterator'Class;

   function Element (Object : Instance; Index : Positive) return Node_Reference;
   function Element (Object : Instance; Position : Cursor) return Node_Reference
     with Pre => Has_Element (Position);

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
      Index : Count_Type;
   end record;

   No_Element : constant Cursor := (Container => null, Index => 0);

   package Constructors is
      function For_Document (Document : not null access Document_Instance)
                             return Instance is (Document => Document, Data => <>)
        with Export, Link_Name => "AdaYaml__Sequence_Data__For_Document";
   end Constructors;
end Yaml.Dom.Sequence_Data;
