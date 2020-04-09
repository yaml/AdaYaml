--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers;
with Ada.Finalization;
with Text.Pool;

generic
   type Value_Type is private;
package Yaml.Text_Set is
   type Reference is new Ada.Finalization.Limited_Controlled with private;

   type Holder is record
      Hash : Ada.Containers.Hash_Type;
      Key : Text.Reference;
      Value : Value_Type;
   end record;

   function Get (Object : in out Reference; S : Standard.String;
                 Create : Boolean) return not null access Holder;
   function Set (Object : in out Reference; S : Standard.String;
                 Value : Value_Type) return Boolean;

   procedure Init (Object : in out Reference; Pool : Text.Pool.Reference;
                   Initial_Size : Positive);
   procedure Clear (Object : in out Reference);
private
   type Holder_Access is access all Holder;
   type Holder_Array is array (Natural range <>) of aliased Holder;
   type Holder_Array_Access is access Holder_Array;

   type Reference is new Ada.Finalization.Limited_Controlled with record
      Count : Natural;
      Elements : Holder_Array_Access;
      Pool : Text.Pool.Reference;
   end record;

   overriding procedure Finalize (Object : in out Reference);
end Yaml.Text_Set;
