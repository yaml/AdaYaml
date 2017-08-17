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
   begin
      if Object.Stream_Count > 0 then
         raise State_Error with
           "cannot manipulate event queue while a Stream_Instance exists";
      end if;
      if Object.Length = Object.Data.all'Length then
         Object.Grow;
         Object.First_Pos := 1;
      end if;
      Object.Data (((Object.First_Pos + Object.Length - 1) mod Object.Data.all'Length) + 1) := E;
      Object.Length := Object.Length + 1;
   end Append;

   function Length (Object : in Instance) return Natural is
     (Object.Length);

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

   function New_Queue return Reference is
      Ptr : constant not null access Instance := new Instance;
   begin
      return Reference'(Ada.Finalization.Controlled with Data => Ptr);
   end New_Queue;

   package body Iteration is
      function As_Stream (Object : Reference) return Stream_Reference is
         Ptr : constant not null access Stream_Instance :=
           new Stream_Instance'(Refcount_Base with
                                Buffer => Object.Data.all'Unchecked_Access,
                                Offset => 0);
      begin
         Increase_Refcount (Ptr);
         Ptr.Buffer.Stream_Count := Ptr.Buffer.Stream_Count + 1;
         return Stream_Reference'(Ada.Finalization.Controlled with
                                    Data => Ptr);
      end As_Stream;
   end Iteration;

   function Next (Object : in out Stream_Instance) return Event is
      Index : constant Positive := (Object.Buffer.First_Pos + Object.Offset) mod
        Object.Buffer.Data.all'Last;
   begin
      if Object.Offset = Object.Buffer.Length then
         raise Constraint_Error with
           "Tried to query more events than in queue";
      else
         return E : constant Event := Object.Buffer.Data (Index) do
            Object.Offset := Object.Offset + 1;
         end return;
      end if;
   end Next;

   procedure Finalize (Object : in out Stream_Instance) is
   begin
      Decrease_Refcount (Object.Buffer);
      Object.Buffer.Stream_Count := Object.Buffer.Stream_Count - 1;
   end Finalize;

   overriding procedure Adjust (Object : in out Stream_Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Stream_Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;
end Yaml.Events.Queue;
