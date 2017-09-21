--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text.Pool;

package Text.Builder is
   type Reference (<>) is new Ada.Finalization.Controlled with private;

   function Create (Pool : in out Text.Pool.Reference;
                    Initial_Size : Positive := 255) return Reference;

   procedure Append (Object : in out Reference; Value : String);
   procedure Append (Object : in out Reference; Value : Character);

   function Lock (Object : in out Reference) return Text.Reference;
private
   type Reference is new Ada.Finalization.Controlled with record
      Pool : Text.Pool.Reference;
      Buffer : not null UTF_8_String_Access;
      Length : System.Storage_Elements.Storage_Offset := 0;
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);
end Text.Builder;
