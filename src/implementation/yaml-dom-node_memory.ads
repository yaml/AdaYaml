--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Indefinite_Hashed_Sets;
private with System.Storage_Elements;
with Yaml.Dom.Node;

private package Yaml.Dom.Node_Memory is
   type Instance is tagged limited private;

   procedure Visit (Object : in out Instance;
                    Value  : not null access constant Node.Instance;
                    Previously_Visited : out Boolean);

   procedure Forget (Object : in out Instance;
                     Value  : not null access constant Node.Instance);

   function Pop_First (Object : in out Instance)
                       return not null access Node.Instance;

   function Is_Empty (Object : Instance) return Boolean;

   type Pair_Instance is tagged limited private;

   procedure Visit (Object : in out Pair_Instance;
                    Left, Right : not null access Node.Instance;
                    Previously_Visited : out Boolean);
private
   function Hash (Value : Node_Pointer) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (System.Storage_Elements.To_Integer (Value.all'Address)));

   package Pointer_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Node_Pointer, Hash, Dom."=");

   type Instance is tagged limited record
      Data : Pointer_Sets.Set;
   end record;

   type Pointer_Pair is record
      Left, Right : not null access Node.Instance;
   end record;

   use type Ada.Containers.Hash_Type;

   function Hash (Value : Pointer_Pair) return Ada.Containers.Hash_Type is
     (Hash (Value.Left) xor Hash (Value.Right));

   package Pair_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Pointer_Pair, Hash, Node_Memory."=", Node_Memory."=");

   type Pair_Instance is tagged limited record
      Data : Pair_Sets.Set;
   end record;
end Yaml.Dom.Node_Memory;
