with Ada.Unchecked_Deallocation;

package body Yaml.Events.Buffers is
   procedure Initialize (Buffer : in out Event_Buffer) is
   begin
      Streams.Create (Buffer, Streams.Implementation_Access'(
                      new Event_Buffer_Implementation));
   end Initialize;

   procedure Append (Buffer : in out Event_Buffer; E : Event) is
      Impl : constant Event_Buffer_Implementation_Access :=
        Event_Buffer_Implementation_Access (Buffer.Implementation);
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

   procedure Fetch (Stream : in out Event_Buffer_Implementation;
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

   procedure Close_Stream (Stream : in out Event_Buffer_Implementation) is
   begin
      Stream.Iterator := 1;
   end Close_Stream;
end Yaml.Events.Buffers;
