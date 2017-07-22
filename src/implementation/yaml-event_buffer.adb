--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Event_Buffer is
   procedure Initialize (Object : in out Reference) is
   begin
      Stream.Create (Object, Stream.Implementation_Pointer'(
                     new Implementation));
   end Initialize;

   procedure Append (Object : in out Reference; E : Event) is
      Impl : constant Implementation_Pointer :=
        Implementation_Pointer (Object.Implementation_Access);
   begin
      if Impl.Last = Impl.Data.all'Last then
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (Event_Array, Event_Array_Access);
            New_Data : constant not null access Event_Array :=
              new Event_Array (1 .. Impl.Data.all'Last * 2);
            Old_Data : Event_Array_Access := Impl.Data;
         begin
            New_Data (1 .. Impl.Data.all'Last) := Impl.Data.all;
            Impl.Data.all := New_Data.all;
            Free (Old_Data);
         end;
      end if;
      Impl.Last := Impl.Last + 1;
      Impl.Data (Impl.Last) := E;
   end Append;

   procedure Fetch (Stream : in out Implementation;
                    E : out Event) is
   begin
      if Stream.Iterator > Stream.Last then
         raise Constraint_Error with
           "Tried to query more events than in buffer";
      else
         E := Stream.Data (Stream.Iterator);
         Stream.Iterator := Stream.Iterator + 1;
      end if;
   end Fetch;

   procedure Close_Stream (Stream : in out Implementation) is
   begin
      Stream.Iterator := 1;
   end Close_Stream;
end Yaml.Event_Buffer;
