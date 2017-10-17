--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

pragma No_Strict_Aliasing;

with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body Text is
   type Header_Access is access Header;
   for Header_Access'Size use Standard'Address_Size;

   function Header_Of (S : UTF_8_String_Access) return not null access Header is
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

   function Hash (Object : Reference) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (Object.Data.all));

   function "=" (Left : String; Right : Reference) return Boolean is
     (Left = Right.Data.all);

   function Element (Object : Reference; Position : Positive) return Character
     is (Object.Data (Position));

   function Hold (Content : String) return Constant_Instance is
      Ret : Constant_Instance :=
        (Length => Content'Length + Positive (Header_Size) + 1,
         Data => <>);
      H : Header with Import;
      for H'Address use Ret.Data (1)'Address;
   begin
      H.Pool := null;
      H.Refcount := 1;
      H.First := 1;
      H.Last := Content'Length;
      Ret.Data (Positive (Header_Size) + 1 .. Ret.Data'Last) :=
        Content & Character'Val (0);
      return Ret;
   end Hold;

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
                  H.Last := Round_To_Header_Size (H.Last + 1);
                  Decrease_Usage (H.Pool, H.Chunk_Index);
               end if;
            end if;
         end;
      end if;
   end Finalize;

   function Held (Holder : Constant_Instance) return Reference is
     ((Reference'(Ada.Finalization.Controlled with
                Data => To_UTF_8_String_Access (Holder.Data (Positive (Header_Size) + 1)'Address))));

   function Export (Object : Reference) return Exported is
      H : constant access Header := Header_Of (Object.Data);
   begin
      H.Refcount := H.Refcount + 1;
      return Object.Data.all'Address;
   end Export;

   function Import (Pointer : Exported) return Reference is
   begin
      return Value : constant Reference :=
        (Ada.Finalization.Controlled with
           Data => To_UTF_8_String_Access (Pointer)) do
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

   function Fitting_Position (Length : Pool_Offset;
                              P : Pool_Data_Access) return System.Address is
      Necessary : constant Pool_Offset :=
        Round_To_Header_Size (Length + 1);
      Cur : Pool_Offset := P.Pos;

      procedure Allocate_Next_Chunk is
         Next : Chunk_Index_Type;
      begin
         for I in Chunk_Index_Type loop
            if P.Chunks (I) = null then
               Next := I;
               goto Found;
            end if;
         end loop;

         raise Storage_Error with "String pool depleted.";

         <<Found>>

         P.Chunks (Next) := new Pool_Array
           (Pool_Offset (1) .. Pool_Offset'Max (
            P.Chunks (P.Cur)'Length * 2,
            Necessary + Header_Size));
         P.Usage (P.Cur) := P.Usage (P.Cur) - 1;
         P.Usage (Next) := 1;
         P.Cur := Next;
         P.Pos := 1;
      end Allocate_Next_Chunk;
   begin
      loop
         declare
            C : constant Chunk := P.Chunks (P.Cur);
            pragma Warnings (Off, C);
            --  no 'may be modified via address clause'
            H : Header with Import;
            for H'Address use C (Cur)'Address;
         begin
            <<Check_Length>>

            if H.Last >= Necessary then
               P.Pos := Cur + Header_Size + Necessary;
               if H.Last > Necessary then
                  declare
                     Next : Header with Import;
                     for Next'Address use C (P.Pos)'Address;
                     use System.Storage_Elements;
                  begin
                     Next.Refcount := 0;
                     Next.Last := H.Last - Necessary - Header_Size;
                  end;
               else
                  loop
                     if P.Pos > C.all'Last then
                        P.Pos := 1;
                     end if;
                     if P.Pos = Cur then
                        Allocate_Next_Chunk;
                        exit;
                     end if;
                     declare
                        Next : Header with Import;
                        for Next'Address use C (P.Pos)'Address;
                     begin
                        exit when Next.Refcount = 0;
                        P.Pos := P.Pos + Header_Size +
                          Round_To_Header_Size (Next.Last + 1);
                     end;
                  end loop;
               end if;
               H.Refcount := 1;
               H.Pool := P;
               H.Chunk_Index := P.Cur;
               H.Last := Length;
               H.First := 1;
               P.Usage (P.Cur) :=
                 P.Usage (P.Cur) + 1;
               return H'Address + Header_Size;
            else
               declare
                  Next_Pos : constant Pool_Offset :=
                    Cur + Header_Size + H.Last;
               begin
                  if Next_Pos <= C.all'Last then
                     declare
                        Next : Header with Import;
                        for Next'Address use C (Next_Pos)'Address;
                     begin
                        if Next.Refcount = 0 then
                           H.Last := H.Last + Header_Size + Next.Last;
                           goto Check_Length;
                        end if;
                     end;
                  end if;
               end;
            end if;
            Cur := Cur + H.Last + Header_Size;
            loop
               if Cur >= C.all'Last then
                  Cur := 1;
               end if;
               if Cur = P.Pos then
                  Allocate_Next_Chunk;
                  exit;
               end if;
               declare
                  Next : Header with Import;
                  for Next'Address use C (Cur)'Address;
               begin
                  exit when Next.Refcount = 0;
                  Cur := Cur + Header_Size + Round_To_Header_Size (Next.Last + 1);
               end;
            end loop;
         end;
      end loop;
   end Fitting_Position;

   function As_String (C : Chunk) return String is
      use Ada.Strings.Unbounded;
      Cur : Pool_Offset := 1;
      Output : Unbounded_String;
   begin
      loop
         declare
            H : Header with Import;
            for H'Address use C (Cur)'Address;
         begin
            Append (Output, "Refcount:" & H.Refcount'Img & Character'Val (10));
            if H.Refcount = 0 then
               Append (Output, "Length:" & H.Last'Img & Character'Val (10));
               Cur := Cur + Header_Size + H.Last;
            else
               Append (Output, "Content: " & To_UTF_8_String_Access (C (Cur + Header_Size)'Address).all);
               Cur := Cur + Header_Size + Round_To_Header_Size (H.Last + 1);
            end if;
         end;
         exit when Cur > C.all'Last;
         Append (Output, Character'Val (10) & Character'Val (10));
      end loop;
      return To_String (Output);
   end As_String;
end Text;
