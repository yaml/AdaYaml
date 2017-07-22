--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;

generic
   type Element_Type is private;
package Yaml.Stacks is
   --  this package provides a reference-counted stack. compared to Ada's
   --  standard container types, it has pointer semantics and is also able to
   --  query access to an element, which is useful for in-place modification.

   type Stack is tagged private;

   function New_Stack (Initial_Capacity : Positive) return Stack;
   function Top (Object : in out Stack) return access Element_Type;
   function Length (Object : Stack) return Natural;
   function Element (Object : Stack; Index : Positive)
                     return access Element_Type;
   procedure Pop (Object : in out Stack);
   procedure Push (Object : in out Stack; Value : Element_Type);
   function Initialized (Object : Stack) return Boolean;
private
   type Element_Array is array (Positive range <>) of aliased Element_Type;
   type Element_Array_Access is access Element_Array;

   type Holder is record
      Elements : Element_Array_Access;
      Refcount : Natural := 1;
      Length   : Natural := 0;
   end record;
   type Holder_Access is access Holder;

   type Stack is new Ada.Finalization.Controlled with record
      Data : Holder_Access := null;
   end record;

   overriding procedure Adjust (Object : in out Stack);
   overriding procedure Finalize (Object : in out Stack);
end Yaml.Stacks;
