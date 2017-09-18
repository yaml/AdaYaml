--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Hashed_Sets;
private with System.Storage_Elements;
with Yaml.Dom.Node;

private package Yaml.Dom.Node_Memory is
   type Instance is tagged limited private;

   procedure Visit (Object : in out Instance;
                    Value  : not null access constant Node.Instance;
                    Previously_Visited : out Boolean);

   procedure Forget (Object : in out Instance;
                     Value  : not null access constant Node.Instance);

   type Pair_Instance is tagged limited private;

   procedure Visit (Object : in out Pair_Instance;
                    Left, Right : not null access constant Node.Instance;
                    Previously_Visited : out Boolean);
private
   function Hash (Value : System.Address) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (System.Storage_Elements.To_Integer (Value)));

   package Address_Sets is new Ada.Containers.Hashed_Sets
     (System.Address, Hash, System."=", System."=");

   type Instance is tagged limited record
      Data : Address_Sets.Set;
   end record;

   type Address_Pair is record
      Left, Right : System.Address;
   end record;

   use type Ada.Containers.Hash_Type;

   function Hash (Value : Address_Pair) return Ada.Containers.Hash_Type is
     (Hash (Value.Left) xor Hash (Value.Right));

   package Pair_Sets is new Ada.Containers.Hashed_Sets
     (Address_Pair, Hash, Node_Memory."=", Node_Memory."=");

   type Pair_Instance is tagged limited record
      Data : Pair_Sets.Set;
   end record;
end Yaml.Dom.Node_Memory;
