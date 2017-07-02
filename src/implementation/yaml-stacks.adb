--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Stacks is
   procedure Adjust (Object : in out Stack) is
   begin
      if Object.Data /= null then
         Object.Data.Refcount := Object.Data.Refcount + 1;
      end if;
   end Adjust;

   procedure Free_Element_Array is new Ada.Unchecked_Deallocation
     (Element_Array, Element_Array_Access);

   procedure Finalize (Object : in out Stack) is
      procedure Free_Holder is new Ada.Unchecked_Deallocation
        (Holder, Holder_Access);
      Reference : Holder_Access := Object.Data;
   begin
      Object.Data := null;
      if Reference /= null then
         Reference.Refcount := Reference.Refcount - 1;
         if Reference.Refcount = 0 then
            Free_Element_Array (Reference.Elements);
            Free_Holder (Reference);
         end if;
      end if;
   end Finalize;

   function New_Stack (Initial_Capacity : Positive) return Stack is
   begin
      return Ret : constant Stack :=
        (Ada.Finalization.Controlled with Data => new Holder) do
         Ret.Data.Elements := new Element_Array (1 .. Initial_Capacity);
         Ret.Data.Length := 0;
      end return;
   end New_Stack;

   function Top (Object : in out Stack) return access Element_Type is
     (Object.Data.Elements (Object.Data.Length)'Access);

   function Length (Object : Stack) return Natural is
     (if Object.Data = null then 0 else Object.Data.Length);

   function Element (Object : Stack; Index : Positive)
                     return access Element_Type is
     (Object.Data.Elements (Index)'Access);

   procedure Pop (Object : in out Stack) is
   begin
      Object.Data.Length := Object.Data.Length - 1;
   end Pop;

   procedure Push (Object : in out Stack; Value : Element_Type) is
   begin
      if Object.Data = null then
         Object := New_Stack (32);
      end if;
      if Object.Data.Length = Object.Data.Elements.all'Last then
         declare
            New_Array : constant Element_Array_Access :=
              new Element_Array (1 .. Object.Data.Length * 2);
         begin
            New_Array (1 .. Object.Data.Length) := Object.Data.Elements.all;
            Free_Element_Array (Object.Data.Elements);
            Object.Data.Elements := New_Array;
         end;
      end if;
      Object.Data.Length := Object.Data.Length + 1;
      Object.Data.Elements (Object.Data.Length) := Value;
   end Push;

   function Initialized (Object : Stack) return Boolean is
      (Object.Data /= null);

end Yaml.Stacks;
