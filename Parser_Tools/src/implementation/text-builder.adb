--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Text.Builder is
   H_Size : constant System.Storage_Elements.Storage_Offset :=
     System.Storage_Elements.Storage_Offset (Header_Size);

   procedure Init (Object : in out Reference; Pool : Text.Pool.Reference;
                   Initial_Size : Positive := 255) is
      Base : constant Text.Reference := Pool.With_Length (Initial_Size);
      H : constant not null access Header := Header_Of (Base.Data);
   begin
      H.Refcount := H.Refcount + 1;
      Object.Next := 1;
      Object.Buffer := Base.Data;
      Object.Pool := Pool;
   end Init;

   function Create (Pool : Text.Pool.Reference;
                    Initial_Size : Positive := 255) return Reference is
   begin
      return Ret : Reference do
         Init (Ret, Pool, Initial_Size);
      end return;
   end Create;

   function Initialized (Object : Reference) return Boolean is
     (Object.Buffer /= null);

   procedure Grow (Object : in out Reference;
                   Size : System.Storage_Elements.Storage_Offset) is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
      Old : constant Text.Reference := (Ada.Finalization.Controlled with
                                        Data => Object.Buffer);
      New_Buffer : constant Text.Reference :=
        Object.Pool.With_Length
          (Positive (((Object.Next + Size + H.Last) /
           (H.Last + 1)) * (H.Last + 1) - 1));
      New_H : constant not null access Header := Header_Of (New_Buffer.Data);
   begin
      New_H.Refcount := New_H.Refcount + 1;
      New_Buffer.Data (1 .. Natural (Object.Next - 1)) :=
        Old.Data (1 .. Natural (Object.Next - 1));
      Object.Buffer := New_Buffer.Data;
   end Grow;

   procedure Append (Object : in out Reference; Value : String) is

   begin
      if Object.Next + Value'Length - 1 >
        System.Storage_Elements.Storage_Offset (Object.Buffer.all'Last) then
         Grow (Object, Value'Length);
      end if;
      Object.Buffer (Positive (Object.Next) .. Natural (Object.Next +
                     Value'Length - 1)) := Value;
      Object.Next := Object.Next + Value'Length;
   end Append;

   procedure Append (Object : in out Reference; Value : Character) is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
   begin
      if Object.Next = H.Last - 1 then
         Grow (Object, 1);
      end if;
      Object.Buffer (Positive (Object.Next)) := Value;
      Object.Next := Object.Next + 1;
   end Append;

   procedure Append (Object : in out Reference; Value : Text.Reference) is
   begin
      Object.Append (Value.Data.all);
   end Append;

   function Lock (Object : in out Reference) return Text.Reference is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
   begin
      H.Last := Object.Next - 1;
      H.Refcount := H.Refcount + 1;
      return (Ada.Finalization.Controlled with Data => Object.Buffer);
   end Lock;

   procedure Adjust (Object : in out Reference) is
   begin
      if Object.Buffer /= null then
         declare
            H : Header with Import;
            for H'Address use Object.Buffer.all'Address - H_Size;
         begin
            H.Refcount := H.Refcount + 1;
         end;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      if Object.Buffer /= null then
         declare
            H : Header with Import;
            for H'Address use Object.Buffer.all'Address - H_Size;
         begin
            H.Refcount := H.Refcount - 1;
            if H.Refcount = 0 then
               H.Last := Round_To_Header_Size (H.Last);
               Decrease_Usage (H.Pool, H.Chunk_Index);
            end if;
         end;
      end if;
   end Finalize;
end Text.Builder;
