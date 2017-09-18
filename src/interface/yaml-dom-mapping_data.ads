--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Indefinite_Hashed_Maps;

package Yaml.Dom.Mapping_Data is
   type Instance is tagged limited private
     with Constant_Indexing => Element;

   type Cursor is private;

   function Length (Object : Instance) return Count_Type;

   function Has_Element (Position : Cursor) return Boolean;

   function First (Object : Instance) return Cursor;

   function Next (Position : Cursor) return Cursor;

   procedure Iterate (Object : Instance;
                      Process : not null access procedure
                        (Key, Value : not null access Node.Instance));

   function Key (Position : Cursor) return Node_Reference
     with Pre => Has_Element (Position);

   function Value (Position : Cursor) return Node_Reference
     with Pre => Has_Element (Position);

   function Find (Object : Instance; Key : Node_Reference) return Cursor;

   function Element (Object : Instance; Key : Node_Reference)
                     return Node_Reference;

   --  convenience method for retrieving values of keys that are scalars.
   --  the key value is given as String.
   function Element (Object : Instance; Key : String) return Node_Reference;

   No_Element : constant Cursor;
private
   function Hash (Object : Node_Pointer) return Ada.Containers.Hash_Type;

   package Node_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Node_Pointer, Node_Pointer, Hash, Dom."=", Dom."=");

   type Instance is tagged limited record
      Document : not null access Document_Instance;
      Data : Node_Maps.Map;
   end record;

   type Cursor is record
      Container : access Instance;
      Position  : Node_Maps.Cursor;
   end record;

   No_Element : constant Cursor := (Container => null,
                                    Position => Node_Maps.No_Element);

   package Constructors is
      function For_Document (Document : not null access Document_Instance)
                             return Instance is (Document => Document, Data => <>)
        with Export, Link_Name => "AdaYaml__Mapping_Data__For_Document";
   end Constructors;
end Yaml.Dom.Mapping_Data;
