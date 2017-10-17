--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Text.Pool is
   type Reference is new Ada.Finalization.Controlled with private;

   Default_Size : constant Pool_Offset;

   --  must be called once before the string pool can be used. if called again,
   --  the string pool re-initializes itself with new memory, and the old memory
   --  lives on only in References that have already been generated. the
   --  old memory is reclaimed once all string references to it vanish.
   procedure Create (P : in out Reference'Class;
                     Initial_Size : Pool_Offset := Default_Size);

   --  constructor that calls Create with the given Size parameter
   function With_Capacity (Size : Pool_Offset) return Reference;

   --  create a new string from the given data. the string will be allocated
   --  within the pool.
   function From_String (P : in out Reference'Class; Data : String)
                         return Text.Reference;

   function With_Length (P : in out Reference'Class; Length : Positive)
                         return Text.Reference;

   --  for debugging
   function Current_Chunk_As_String (P : Reference) return String;
private
   type Reference is new Ada.Finalization.Controlled with record
      Data : Pool_Data_Access;
   end record with Type_Invariant =>
     (Reference.Data = null or else Reference.Data.Pos mod Header_Size = 1);

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   Default_Size : constant Pool_Offset := 8192;
end Text.Pool;
