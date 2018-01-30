--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text.Pool;

package Text.Builder is
   type Reference is limited new Ada.Finalization.Limited_Controlled
     with private;

   procedure Init (Object : in out Reference; Pool : Text.Pool.Reference;
                   Initial_Size : Positive := 255);

   function Create (Pool : Text.Pool.Reference;
                    Initial_Size : Positive := 255) return Reference;

   function Initialized (Object : Reference) return Boolean;

   procedure Append (Object : in out Reference; Value : String)
     with Pre => Object.Initialized;

   procedure Append (Object : in out Reference; Value : Character)
     with Pre => Object.Initialized;

   procedure Append (Object : in out Reference; Value : Text.Reference)
     with Pre => Object.Initialized;

   function Lock (Object : in out Reference) return Text.Reference;
private
   type Reference is limited new Ada.Finalization.Limited_Controlled with record
      Pool : Text.Pool.Reference;
      Buffer : UTF_8_String_Access;
      Next : System.Storage_Elements.Storage_Offset := 1;
   end record;

   overriding procedure Finalize (Object : in out Reference);

   procedure Grow (Object : in out Reference;
                   Size : System.Storage_Elements.Storage_Offset);
end Text.Builder;
