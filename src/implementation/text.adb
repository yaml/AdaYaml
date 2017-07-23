--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Text is
   type Header_Access is access Header;
   for Header_Access'Size use Standard'Address_Size;

   function Header_Of (S : UTF_8_String_Access) return access Header
     with Inline is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Header_Access);
      use System.Storage_Elements;
   begin
      return Convert (S.all'Address - Storage_Offset (Header_Size));
   end Header_Of;

   function Value (Object : Reference) return Accessor is
     (Accessor'(Data => Object.Data, Hold => Object));

   function Length (Object : Reference) return Natural is
     (Object.Data'Length);

   function "&" (Left, Right : Reference) return String is
     (Left.Data.all & Right.Data.all);

   function "&" (Left : Reference; Right : String) return String is
     (Left.Data.all & Right);

   function "&" (Left : Reference; Right : Character) return String is
     (Left.Data.all & Right);

   function "&" (Left : String; Right : Reference) return String is
     (Left & Right.Data.all);

   function "&" (Left : Character; Right : Reference) return String is
     (Left & Right.Data.all);

   function "=" (Left, Right : Reference) return Boolean is
     (Left.Data = Right.Data or else
        (Left.Data /= null and then Right.Data /= null and then
         Left.Data.all = Right.Data.all));

   function "=" (Left : Reference; Right : String) return Boolean is
     (Left.Data.all = Right);

   function "=" (Left : String; Right : Reference) return Boolean is
     (Left = Right.Data.all);

   function Element (Object : Reference; Position : Positive) return Character
     is (Object.Data (Position));

   procedure Adjust (Object : in out Reference) is
   begin
      if Object.Data /= null then
         declare
            H : constant access Header := Header_Of (Object.Data);
         begin
            if H.Pool /= null then
               H.Refcount := H.Refcount + 1;
            end if;
         end;
      end if;
   end Adjust;

   procedure Decrease_Usage (Pool : in out Pool_Data_Access;
                             Chunk_Index : Chunk_Index_Type) is
      procedure Free_Chunk is new Ada.Unchecked_Deallocation
        (Pool_Array, Chunk);
      procedure Free_Data is new Ada.Unchecked_Deallocation
        (Pool_Data, Pool_Data_Access);
   begin
      Pool.Usage (Chunk_Index) := Pool.Usage (Chunk_Index) - 1;
      if Pool.Usage (Chunk_Index) = 0 then
         Free_Chunk (Pool.Chunks (Chunk_Index));
         for I in Chunk_Index_Type loop
            if Pool.Chunks (I) /= null then
               return;
            end if;
         end loop;
         Free_Data (Pool);
      end if;
   end Decrease_Usage;

   procedure Finalize (Object : in out Reference) is
      Reference : constant UTF_8_String_Access := Object.Data;
   begin
      Object.Data := null;
      if Reference /= null then
         declare
            H : constant access Header := Header_Of (Reference);
         begin
            if H.Pool /= null then
               H.Refcount := H.Refcount - 1;
               if H.Refcount = 0 then
                  H.Last := Round_To_Header_Size (H.Last);
                  Decrease_Usage (H.Pool, H.Chunk_Index);
               end if;
            end if;
         end;
      end if;
   end Finalize;

   function Held (Holder : Constant_Instance) return Reference is
     ((Reference'(Ada.Finalization.Controlled with
                Data => To_UTF_8_String_Access (Holder.S'Address))));

   function Export (Object : Reference) return Exported is
      H : constant access Header := Header_Of (Object.Data);
   begin
      H.Refcount := H.Refcount + 1;
      return Object.Data.all'Address;
   end Export;

   function Import (Pointer : Exported) return Reference is
      function Convert is new Ada.Unchecked_Conversion
        (Exported, UTF_8_String_Access);
   begin
      return Value : constant Reference := (Ada.Finalization.Controlled with
                                            Data => Convert (Pointer)) do
         declare
            H : constant access Header := Header_Of (Value.Data);
         begin
            H.Refcount := H.Refcount + 1;
         end;
      end return;
   end Import;

   procedure Delete_Exported (Pointer : Exported) is
      use System.Storage_Elements;
      H : Header with Import;
      for H'Address use Pointer - Storage_Offset (Header_Size);
   begin
      H.Refcount := H.Refcount - 1;
      if H.Refcount = 0 then
         H.Last := Round_To_Header_Size (H.Last);
         Decrease_Usage (H.Pool, H.Chunk_Index);
      end if;
   end Delete_Exported;

end Text;
