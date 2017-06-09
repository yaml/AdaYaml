with Ada.Unchecked_Deallocation;

package body Yaml.Streams is
   procedure Adjust (Stream : in out Event_Stream) is
   begin
      if Stream.Implementation /= null then
         Stream.Implementation.Refcount := Stream.Implementation.Refcount + 1;
      end if;
   end Adjust;

   procedure Finalize (Stream : in out Event_Stream) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Stream_Implementation'Class, Implementation_Access);
      Reference : Implementation_Access := Stream.Implementation;
   begin
      Stream.Implementation := null;
      if Reference /= null then
         Reference.Refcount := Reference.Refcount - 1;
         if Reference.Refcount = 0 then
            Reference.Close_Stream;
            Free (Reference);
         end if;
      end if;
   end Finalize;

   function Next (Stream : in out Event_Stream'Class) return Events.Event is
   begin
      if Stream.Implementation.Peeked then
         Stream.Implementation.Peeked := False;
      else
         Fetch (Stream.Implementation.all,
                Stream.Implementation.Cached);
      end if;
      return Stream.Implementation.Cached;
   end Next;

   function Peek (Stream : in out Event_Stream'Class) return Events.Event is
   begin
      return E : constant Events.Event := Next (Stream) do
         Stream.Implementation.Peeked := True;
      end return;
   end Peek;

   procedure Create (Stream : in out Event_Stream'Class;
                     Implementation : Implementation_Access) is
   begin
      Stream.Implementation := Implementation;
      Implementation.Refcount := 1;
   end Create;
end Yaml.Streams;
