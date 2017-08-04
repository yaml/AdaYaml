--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Text.Pool is
   procedure Create (P : in out Reference'Class; Initial_Size : Pool_Offset)
   is
     Initial_Chunk : constant Chunk := new Pool_Array
          (Pool_Offset (1) .. Round_To_Header_Size (Initial_Size));
   begin
      if P.Data /= null then
         Finalize (P);
      end if;
      P.Data := new Pool_Data;
      P.Data.Chunks (1) := Initial_Chunk;
      P.Data.Pos := 1;
      declare
         H : Header with Import;
         for H'Address use Initial_Chunk.all (1)'Address;
      begin
         H.Refcount := 0;
         H.Last := Pool_Offset (Initial_Chunk'Last) - Header_Size;
      end;
   end Create;

   function From_String (P : in out Reference'Class; Data : String)
                         return Text.Reference is
      New_String_Address : constant System.Address :=
        Fitting_Position (Data'Length, P.Data);
      Target_Data : constant UTF_8_String_Access :=
        To_UTF_8_String_Access (New_String_Address);
      Null_Terminator : Character with Import;
      for Null_Terminator'Address use New_String_Address + Data'Length;
   begin
      Null_Terminator := Character'Val (0);
      Target_Data.all := UTF_8_String (Data);
      return (Ada.Finalization.Controlled with Data => Target_Data);
   end From_String;

   function With_Length (P : in out Reference'Class; Length : Positive)
                         return Text.Reference is
      use System.Storage_Elements;
      New_String_Address : constant System.Address :=
        Fitting_Position (Storage_Offset (Length), P.Data);
      Target_Data : constant UTF_8_String_Access :=
        To_UTF_8_String_Access (New_String_Address);
      Null_Terminator : Character with Import;
      for Null_Terminator'Address use New_String_Address + Storage_Offset (Length);
   begin
      Null_Terminator := Character'Val (0);
      return (Ada.Finalization.Controlled with Data => Target_Data);
   end With_Length;

   procedure Adjust (Object : in out Reference) is
   begin
      if Object.Data /= null then
         Object.Data.Refcount := Object.Data.Refcount + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Reference) is
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

   function Current_Chunk_As_String (P : Reference) return String is
      C : constant Chunk := P.Data.Chunks (P.Data.Cur);
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
end Text.Pool;
