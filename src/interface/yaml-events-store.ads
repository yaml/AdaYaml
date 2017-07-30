--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Hashed_Maps;

package Yaml.Events.Store is
   type Instance is limited new Refcount_Base with private;
   type Reference (Data : not null access Instance) is tagged private;

   type Anchored_Position is private;

   procedure Memorize (Object : in out Instance; Item : Event);
   function Position (Object : Instance; Tag : Text.Reference)
                      return Anchored_Position;

   No_Element : constant Anchored_Position;

   type Stream_Instance is limited new Refcount_Base with private;
   type Stream_Reference (Data : not null access Stream_Instance) is
     tagged private;

   function Next (Object : in out Stream_Instance) return Event;

   package Iteration is
      function Retrieve (Object : not null access Instance;
                         Position : Anchored_Position) return Stream_Reference
        with Pre => Position /= No_Element;
   end Iteration;
private
   package Tag_To_Index is new Ada.Containers.Hashed_Maps
     (Text.Reference, Positive, Text.Hash, Text."=");

   type Instance is limited new Event_Holder with record
      Tag_Map : Tag_To_Index.Map;
      Stream_Count : Natural := 0;
      Depth : Natural := 0;
   end record;

   type Reference (Data : not null access Instance) is new
     Ada.Finalization.Controlled with null record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   type Anchored_Position is new Natural;

   No_Element : constant Anchored_Position := 0;

   type Stream_Instance is limited new Refcount_Base with record
      Object  : not null access Instance;
      Depth   : Natural;
      Current : Positive;
   end record;

   overriding procedure Finalize (Object : in out Stream_Instance);

   type Stream_Reference (Data : not null access Stream_Instance) is new
     Ada.Finalization.Controlled with null record;

   overriding procedure Adjust (Object : in out Stream_Reference);
   overriding procedure Finalize (Object : in out Stream_Reference);
end Yaml.Events.Store;
