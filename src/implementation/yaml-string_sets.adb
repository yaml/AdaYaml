with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package body Yaml.String_Sets is
   use type Ada.Containers.Hash_Type;

   function Non_Zero_Hash (S : String) return Ada.Containers.Hash_Type is
      Hash : constant Ada.Containers.Hash_Type := Ada.Strings.Hash (S);
   begin
      if Hash = 0 then
         return 1;
      else
         return Hash;
      end if;
   end Non_Zero_Hash;

   function Raw_Set (Object : in out String_Set;
                     Hash : Ada.Containers.Hash_Type;
                     S : String)
                     return not null access Holder is
      Pos : Natural :=
        Natural (Hash mod Ada.Containers.Hash_Type (Object.Elements'Length));
      Cur : not null access Holder := Object.Elements (Pos)'Access;
   begin
      while Cur.Hash /= 0 and then
        (Cur.Hash /= Hash or else Strings.Value (Cur.Key) /= S) loop
         Pos := Pos + 1;
         if Pos = Object.Elements'Length then
            Pos := 0;
         end if;
         Cur := Object.Elements (Pos)'Access;
      end loop;
      return Cur;
   end Raw_Set;

   procedure Free is new Ada.Unchecked_Deallocation
     (Holder_Array, Holder_Array_Access);

   function Grow_If_Needed (Object : in out String_Set) return Boolean is
      Old_Elements : Holder_Array_Access := Object.Elements;
   begin
      if Object.Count = Object.Elements'Length / 2 then
         Object.Elements := new Holder_Array (0 .. Object.Count * 4 - 1);
         Object.Elements.all := (others => (Hash => 0, others => <>));
         for E of Old_Elements.all loop
            if E.Hash /= 0 then
               Raw_Set (Object, E.Hash, Strings.Value (E.Key)).all := E;
            end if;
         end loop;
         Free (Old_Elements);
         return True;
      else
         return False;
      end if;
   end Grow_If_Needed;

   function Get (Object : in out String_Set; S : String; Create : Boolean)
                 return not null access Holder is
      Hash : constant Ada.Containers.Hash_Type := Non_Zero_Hash (S);
   begin
      <<Start>>
      declare
         Cur : constant not null access Holder := Raw_Set (Object, Hash, S);
      begin
         if Cur.Hash = 0 then
            if Grow_If_Needed (Object) then
               goto Start;
            end if;
            if Create then
               Object.Count := Object.Count + 1;
               Cur.Hash := Hash;
               Cur.Key := Strings.From_String (Object.Pool, S);
            end if;
         end if;
         return Cur;
      end;
   end Get;

   function Set (Object : in out String_Set;
                 S : String; Value : Value_Type) return Boolean is
      Hash : constant Ada.Containers.Hash_Type := Non_Zero_Hash (S);
   begin
      if Grow_If_Needed (Object) then null; end if;
      declare
         Cur : constant not null access Holder := Raw_Set (Object, Hash, S);
      begin
         if Cur.Hash = 0 then
            Object.Count := Object.Count + 1;
            Cur.Hash := Hash;
            Cur.Key := Strings.From_String (Object.Pool, S);
            Cur.Value := Value;
            return True;
         else
            return False;
         end if;
      end;
   end Set;

   procedure Clear (Object : in out String_Set) is
   begin
      Object.Elements.all := (others => (Hash => 0, others => <>));
      Object.Count := 0;
   end Clear;

   procedure Init (Object : in out String_Set; Pool : Strings.String_Pool;
                   Initial_Size : Positive) is
   begin
      Object.Pool := Pool;
      Object.Elements := new Holder_Array (0 .. Initial_Size - 1);
      Clear (Object);
   end Init;

   procedure Finalize (Object : in out String_Set) is
   begin
      if Object.Elements /= null then
         Free (Object.Elements);
      end if;
   end Finalize;
end Yaml.String_Sets;
