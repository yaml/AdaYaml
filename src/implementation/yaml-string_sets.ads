--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Strings;
with Ada.Containers;
with Ada.Finalization;

generic
   type Value_Type is private;
package Yaml.String_Sets is

   type String_Set is new Ada.Finalization.Limited_Controlled with private;

   type Holder is record
      Hash : Ada.Containers.Hash_Type;
      Key : Strings.Content;
      Value : Value_Type;
   end record;

   function Get (Object : in out String_Set; S : String; Create : Boolean)
                 return not null access Holder;
   function Set (Object : in out String_Set; S : String; Value : Value_Type)
                 return Boolean;

   procedure Init (Object : in out String_Set; Pool : Strings.String_Pool;
                   Initial_Size : Positive);
   procedure Clear (Object : in out String_Set);
private
   type Holder_Array is array (Natural range <>) of aliased Holder;
   type Holder_Array_Access is access Holder_Array;

   type String_Set is new Ada.Finalization.Limited_Controlled with record
      Count : Natural;
      Elements : Holder_Array_Access;
      Pool : Strings.String_Pool;
   end record;

   overriding procedure Finalize (Object : in out String_Set);
end Yaml.String_Sets;
