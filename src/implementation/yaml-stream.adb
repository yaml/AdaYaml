--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Stream is
   procedure Adjust (Object : in out Reference) is
   begin
      if Object.Impl /= null then
         Object.Impl.Refcount := Object.Impl.Refcount + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Reference) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Implementation'Class, Implementation_Pointer);
      Ptr : Implementation_Pointer := Object.Impl;
   begin
      Object.Impl := null;
      if Ptr /= null then
         Ptr.Refcount := Ptr.Refcount - 1;
         if Ptr.Refcount = 0 then
            Ptr.Close_Stream;
            Free (Ptr);
         end if;
      end if;
   end Finalize;

   function Next (S : in out Reference'Class) return Event is
   begin
      if S.Impl.Peeked then
         S.Impl.Peeked := False;
      else
         Fetch (S.Impl.all,
                S.Impl.Cached);
      end if;
      return S.Impl.Cached;
   end Next;

   function Peek (S : in out Reference'Class) return Event is
   begin
      return E : constant Event := Next (S) do
         S.Impl.Peeked := True;
      end return;
   end Peek;

   procedure Create (S : in out Reference'Class;
                     Impl : Implementation_Pointer) is
   begin
      S.Impl := Impl;
      Impl.Refcount := 1;
   end Create;

   function Implementation_Access (S : Reference'Class)
                                  return Implementation_Pointer is
      (S.Impl);
end Yaml.Stream;
