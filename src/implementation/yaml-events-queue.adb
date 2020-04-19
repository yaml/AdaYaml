--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Events.Queue is
   procedure Adjust (Object : in out Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;

   procedure Copy_Data (Source : Instance;
                        Target : not null Event_Array_Access) is
   begin
      Target (1 .. Source.Data.all'Last - Source.First_Pos + 1) :=
        Source.Data (Source.First_Pos .. Source.Data.all'Last);
      Target (Source.Data.all'Last - Source.First_Pos + 2 ..
                Source.Data.all'Last) :=
          Source.Data (1 .. Source.First_Pos - 1);
   end Copy_Data;

   procedure Append (Object : in out Instance; E : Event) is
      Position : Mark;
   begin
      Append (Object, E, Position);
   end Append;

   function Next_Index (Object : Instance) return Positive is
      (((Object.First_Pos + Object.Length - 1) mod
                 Object.Data.all'Length) + 1);

   procedure Append (Object : in out Instance; E : Event; Position : out Mark)
   is begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      end if;
      if Object.Length = Object.Data.all'Length then
         Object.Grow;
         Object.First_Pos := 1;
      end if;
      Position := Mark (Next_Index (Object));
      Object.Data (Positive (Position)) := E;
      Object.Length := Object.Length + 1;
   end Append;

   function Length (Object : in Instance) return Natural is
     (Object.Length);

   function Length (Object : Reference) return Natural is
     (Object.Data.Length);

   function First (Object : in Instance) return Event is
   begin
      if Object.Length = 0 then
         raise Constraint_Error with "no items in event queue";
      end if;
      return Object.Data (Object.First_Pos);
   end First;

   procedure Dequeue (Object : in out Instance) is
   begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      elsif Object.Length = 0 then
         raise Constraint_Error with "no items in event queue";
      end if;
      Object.Length := Object.Length - 1;
      Object.First_Pos := Object.First_Pos + 1;
      if Object.First_Pos > Object.Data.all'Last then
         Object.First_Pos := 1;
      end if;
   end Dequeue;

   function Value (Object : Reference) return Accessor is
     ((Data => Object.Data));

   function New_Queue return Reference is
      Ptr : constant not null Instance_Access := new Instance;
   begin
      return Reference'(Ada.Finalization.Controlled with Data => Ptr);
   end New_Queue;

   function Element (Object : Instance; Position : Mark)
                     return Element_Accessor is
      Index : constant Positive := Positive (Position);
   begin
      if Index < Object.First_Pos and then Index >= Next_Index (Object) then
         raise State_Error with "no element at this index";
      end if;
      return (Data => Object.Data (Index)'Unrestricted_Access);
   end Element;

   function As_Stream (Object : Reference'Class) return Stream_Reference is
      Ptr : constant not null Stream_Instance_Access :=
        new Stream_Instance'(Refcount_Base with Buffer => Reference (Object),
                             Offset => 0);
   begin
      Ptr.Buffer.Data.Stream_Count := Ptr.Buffer.Data.Stream_Count + 1;
      return Stream_Reference'(Ada.Finalization.Controlled with
                                 Data => Ptr);
   end As_Stream;

   function Value (Object : Stream_Reference) return Stream_Accessor is
     ((Data => Object.Data));

   function Value (Object : Optional_Stream_Reference) return Stream_Accessor is
     ((Data => Object.Data));

   function Next (Object : in out Stream_Instance) return Event is
      Index : constant Positive :=
        (Object.Buffer.Data.First_Pos + Object.Offset) mod
        Object.Buffer.Data.Data.all'Last;
   begin
      if Object.Offset = Object.Buffer.Data.Length then
         raise Constraint_Error with
           "Tried to query more events than in queue";
      else
         return E : constant Event := Object.Buffer.Data.Data (Index) do
            Object.Offset := Object.Offset + 1;
         end return;
      end if;
   end Next;

   function Required (Object : Optional_Stream_Reference'Class)
                      return Stream_Reference is
   begin
      Object.Data.Increase_Refcount;
      return (Ada.Finalization.Controlled with Data => Object.Data);
   end Required;

   function Optional (Object : Stream_Reference'Class)
                     return Optional_Stream_Reference is
   begin
      Object.Data.Increase_Refcount;
      return (Ada.Finalization.Controlled with Data => Object.Data);
   end Optional;

   procedure Finalize (Object : in out Stream_Instance) is
   begin
      Object.Buffer.Data.Stream_Count := Object.Buffer.Data.Stream_Count - 1;
   end Finalize;

   overriding procedure Adjust (Object : in out Stream_Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Stream_Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;

   overriding procedure Adjust (Object : in out Optional_Stream_Reference) is
   begin
      if Object.Data /= null then
         Increase_Refcount (Object.Data);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Optional_Stream_Reference) is
   begin
      if Object.Data /= null then
         Decrease_Refcount (Object.Data);
      end if;
   end Finalize;
end Yaml.Events.Queue;
