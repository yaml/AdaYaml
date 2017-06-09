with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Yaml.Strings is
   use System;
   use System.Storage_Elements;

   --  this is the dope vector of each string allocated in a Chunk. it is put
   --  immediately before the string's value. note that the First and Last
   --  elements are at the exact positions where GNAT searches for the string's
   --  boundary dope. this allows us to access those values for maintaining the
   --  ring list.
   type Header is record
      Pool : Pool_Data_Access;
      Chunk_Index : Chunk_Index_Type;
      Refcount : Refcount_Type := 1;
      First, Last : Storage_Offset;
   end record;

   Chunk_Index_Start : constant := Standard'Address_Size;
   Refcount_Start    : constant := Standard'Address_Size + 8;
   First_Start       : constant := Standard'Address_Size + 32;
   Last_Start        : constant := First_Start + Integer'Size;
   Header_End        : constant := Last_Start + Integer'Size - 1;

   for Header use record
      Pool        at 0 range 0 .. Chunk_Index_Start - 1;
      Chunk_Index at 0 range Chunk_Index_Start .. Refcount_Start - 1;
      Refcount    at 0 range Refcount_Start .. First_Start - 1;
      First       at 0 range First_Start .. Last_Start - 1;
      Last        at 0 range Last_Start .. Header_End;
   end record;
   for Header'Size use Header_End + 1;

   type Header_Access is access Header;
   for Header_Access'Size use Standard'Address_Size;

   Header_Size : constant Storage_Offset := Header'Size / Storage_Unit;

   --  it is important that all allocated strings are aligned to the header
   --  length. else, it may happen that we generate a region of free memory that
   --  is not large enough to hold a header – but we need to write the header
   --  there to hold information for the ring list. therefore, whenever we
   --  calculate offsets, we use this to round them up to a multiple of
   --  Header_Size.
   function Round_To_Header_Size (Length : Storage_Offset)
                                  return Storage_Offset is
     ((Length + Header_Size - 1) / Header_Size * Header_Size);

   procedure Create (Pool : in out String_Pool;
                     Initial_Size : System.Storage_Elements.Storage_Count)
   is
     Initial_Chunk : constant Chunk := new Storage_Array
          (1 .. Round_To_Header_Size (Initial_Size));
   begin
      if Pool.Data /= null then
         Finalize (Pool);
      end if;
      Pool.Data := new Pool_Data;
      Pool.Data.Chunks (1) := Initial_Chunk;
      Pool.Data.Pos := 1;
      declare
         H : Header with Import;
         for H'Address use Initial_Chunk.all (1)'Address;
      begin
         H.Refcount := 0;
         H.Last := Initial_Chunk'Last - Header_Size;
      end;
   end Create;

   function Header_Of (S : UTF_8_String_Access) return access Header
     with Inline is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Header_Access);
   begin
      return Convert (S.all'Address - Header_Size);
   end Header_Of;

   function Value (Object : Content) return Accessor is
     (Accessor'(Data => Object.Data, Hold => Object));

   function "=" (Left, Right : Content) return Boolean is
     (Left.Data = Right.Data or else
        (Left.Data /= null and then Right.Data /= null and then
         Left.Data.all = Right.Data.all));

   function From_String (Pool : in out String_Pool'Class; Data : String)
                         return Content is
      Necessary : constant Storage_Offset :=
        Round_To_Header_Size (Data'Length);

      function Fitting_Position return System.Address is
         Cur : Storage_Offset := Pool.Data.Pos;
      begin
         loop
            declare
               C : constant Chunk := Pool.Data.Chunks (Pool.Data.Cur);
               pragma Warnings (Off, C);
                 --  no 'may be modified via address clause'
               H : Header with Import;
               for H'Address use C (Cur)'Address;
            begin
               if H.Last >= Necessary then
                  if H.Last > Necessary then
                     Pool.Data.Pos := Cur + Necessary + Header_Size;
                     declare
                        Next : Header with Import;
                        for Next'Address use C (Pool.Data.Pos)'Address;
                     begin
                        Next.Refcount := 0;
                        Next.Last := H.Last - Data'Length - Header_Size;
                     end;
                  end if;
                  H.Refcount := 1;
                  H.Pool := Pool.Data;
                  H.Chunk_Index := Pool.Data.Cur;
                  H.Last := Data'Length;
                  H.First := 1;
                  Pool.Data.Usage (Pool.Data.Cur) :=
                    Pool.Data.Usage (Pool.Data.Cur) + 1;
                  return H'Address + Header_Size;
               end if;
               Cur := Cur + Round_To_Header_Size (H.Last) + Header_Size;
            end;
            if Cur >= Pool.Data.Chunks (Pool.Data.Cur)'Last then
               Cur := 1;
            end if;
            if Cur = Pool.Data.Pos then
               declare
                  Next : Chunk_Index_Type;
               begin
                  for I in Chunk_Index_Type loop
                     if Pool.Data.Chunks (I) = null then
                        Next := I;
                        goto Found;
                     end if;
                  end loop;

                  raise Storage_Error with "String pool depleted.";

                  <<Found>>

                  Pool.Data.Chunks (Next) := new Storage_Array
                    (1 .. Storage_Offset'Max (
                     Pool.Data.Chunks (Pool.Data.Cur)'Length * 2,
                     Necessary + Header_Size));
                  Pool.Data.Cur := Next;
                  Pool.Data.Pos := 1;
               end;
            end if;
         end loop;
      end Fitting_Position;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, UTF_8_String_Access);

      Copied_Data : constant UTF_8_String_Access := Convert (Fitting_Position);
   begin
      Copied_Data.all := UTF_8_String (Data);
      return (Ada.Finalization.Controlled with Data => Copied_Data);
   end From_String;

   procedure Adjust (Object : in out Content) is
   begin
      if Object.Data /= null then
         declare
            H : constant access Header := Header_Of (Object.Data);
         begin
            H.Refcount := H.Refcount + 1;
         end;
      end if;
   end Adjust;

   procedure Decrease_Usage (Pool : in out Pool_Data_Access;
                             Chunk_Index : Chunk_Index_Type) is
      procedure Free_Chunk is new Ada.Unchecked_Deallocation
        (System.Storage_Elements.Storage_Array, Chunk);
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

   procedure Finalize (Object : in out Content) is
      Reference : constant UTF_8_String_Access := Object.Data;
   begin
      Object.Data := null;
      if Reference /= null then
         declare
            H : constant access Header := Header_Of (Reference);
         begin
            H.Refcount := H.Refcount - 1;
            if H.Refcount = 0 then
               declare
                  C : constant Chunk := H.Pool.Chunks (H.Chunk_Index);
                  Pos : constant Storage_Offset := H.all'Address - C (1)'Address;
               begin
                  while H.Last + Pos < H.Pool.Chunks (H.Chunk_Index)'Last loop
                     declare
                        Next : Header with Import;
                        for Next'Address use C (Pos + Round_To_Header_Size (H.Last))'Address;
                     begin
                        if Next.Refcount = 0 then
                           H.Last := H.Last + Header_Size + Next.Last;
                        else
                           exit;
                        end if;
                     end;
                  end loop;
               end;
               Decrease_Usage (H.Pool, H.Chunk_Index);
            end if;
         end;
      end if;
   end Finalize;

   procedure Adjust (Object : in out String_Pool) is
   begin
      if Object.Data /= null then
         Object.Data.Refcount := Object.Data.Refcount + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out String_Pool) is
      Reference : Pool_Data_Access := Object.Data;
   begin
      Object.Data := null;
      if Reference /= null then
         Reference.Refcount := Reference.Refcount - 1;
         if Reference.Refcount = 0 then
            --  no reference around; take away Usage + 1 from the current Chunk.
            --  this enables the whole pool data to be freed once all Content
            --  references vanish.
            Decrease_Usage (Reference, Reference.Cur);
         end if;
      end if;
   end Finalize;

   function Current_Chunk_As_String (Pool : String_Pool) return String is
      C : constant Chunk := Pool.Data.Chunks (Pool.Data.Cur);
      function Hex (Val : Integer) return Character is
        (if Val < 10 then Character'Val (Val + Character'Pos ('0')) else
            Character'Val (Val - 10 + Character'Pos ('A')));
   begin
      return Ret : String (1 .. Positive (C.all'Last) * 2) do
         for I in C.all'Range loop
            Ret (Integer (I) * 2 - 1) := Hex (Integer (C (I)) / 16);
            Ret (Integer (I) * 2) := Hex (Integer (C (I)) mod 16);
         end loop;
      end return;
   end Current_Chunk_As_String;
end Yaml.Strings;
