--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Text.Builder is
   H_Size : constant System.Storage_Elements.Storage_Offset :=
     System.Storage_Elements.Storage_Offset (Header_Size);

   function Create (Pool : in out Text.Pool.Reference;
                    Initial_Size : Positive := 255) return Reference is
      Base : constant Text.Reference := Pool.With_Length (Initial_Size);
      H : constant not null access Header := Header_Of (Base.Data);
   begin
      H.Refcount := H.Refcount + 1;
      return (Ada.Finalization.Controlled with Buffer => Base.Data,
              Length => 0, Pool => Pool);
   end Create;

   procedure Grow (Object : in out Reference;
                   Size : System.Storage_Elements.Storage_Offset) is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
      Old : constant Text.Reference := (Ada.Finalization.Controlled with
                                        Data => Object.Buffer);
      New_Buffer : constant Text.Reference :=
        Object.Pool.With_Length
          (Positive (((Object.Length + Size + H.Last + 1) /
           (H.Last + 1)) * (H.Last + 1) - 1));
      New_H : constant not null access Header := Header_Of (New_Buffer.Data);
   begin
      New_H.Refcount := New_H.Refcount + 1;
      New_Buffer.Data (1 .. Positive (Object.Length)) :=
        Old.Data.all;
      Object.Buffer := New_Buffer.Data;
   end Grow;

   procedure Append (Object : in out Reference; Value : String) is

   begin
      if Object.Length + Value'Length >
        System.Storage_Elements.Storage_Offset (Object.Buffer.all'Last) then
         Grow (Object, Value'Length);
      end if;
      Object.Buffer (Positive (Object.Length + 1) .. Natural (Object.Length +
                     Value'Length)) := Value;
      Object.Length := Object.Length + Value'Length;
   end Append;

   procedure Append (Object : in out Reference; Value : Character) is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
   begin
      if Object.Length = H.Last then
         Grow (Object, 1);
      end if;
      Object.Buffer (Positive (Object.Length) + 1) := Value;
      Object.Length := Object.Length + 1;
   end Append;

   function Lock (Object : in out Reference) return Text.Reference is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
   begin
      H.Last := Object.Length;
      H.Refcount := H.Refcount + 1;
      return (Ada.Finalization.Controlled with Data => Object.Buffer);
   end Lock;

   procedure Adjust (Object : in out Reference) is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
   begin
      H.Refcount := H.Refcount + 1;
   end Adjust;

   procedure Finalize (Object : in out Reference) is
      H : Header with Import;
      for H'Address use Object.Buffer.all'Address - H_Size;
   begin
      H.Refcount := H.Refcount - 1;
      if H.Refcount = 0 then
         H.Last := Round_To_Header_Size (H.Last);
         Decrease_Usage (H.Pool, H.Chunk_Index);
      end if;
   end Finalize;
end Text.Builder;
