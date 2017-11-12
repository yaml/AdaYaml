--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Hashed_Maps;

package Yaml.Events.Store is
   type Instance is limited new Refcount_Base with private;
   type Reference is tagged private;
   type Optional_Reference is tagged private;

   type Accessor (Data : not null access Instance) is limited null record with
     Implicit_Dereference => Data;

   type Cursor is private;

   function New_Store return Reference;
   function Value (Object : Reference) return Accessor;

   Null_Reference : constant Optional_Reference;

   function Value (Object : Optional_Reference) return Accessor with
     Pre => Object /= Null_Reference;

   function Optional (Object : Reference'Class) return Optional_Reference;
   function Required (Object : Optional_Reference'Class) return Reference;

   procedure Memorize (Object : in out Instance; Item : Event);
   function Find (Object : Instance; Alias : Text.Reference)
                  return Cursor;
   function Exists_In_Output (Position : Cursor) return Boolean;
   procedure Set_Exists_In_Output (Object : in out Instance; Position : Cursor);

   No_Element : constant Cursor;

   procedure Clear (Object : in out Instance);
   procedure Copy (Source : in Instance; Target : in out Instance);

   type Stream_Instance is limited new Refcount_Base with private;
   type Stream_Reference is tagged private;
   type Optional_Stream_Reference is tagged private;
   type Stream_Accessor (Data : not null access Stream_Instance) is limited
     null record with Implicit_Dereference => Data;

   function Value (Object : Stream_Reference) return Stream_Accessor;
   function Next (Object : in out Stream_Instance) return Event;

   function Exists (Object : Optional_Stream_Reference) return Boolean;
   function Value (Object : Optional_Stream_Reference) return Stream_Accessor
     with Pre => Object.Exists;

   function Optional (Object : Stream_Reference'Class)
                      return Optional_Stream_Reference with
     Post => Optional'Result.Exists;

   procedure Clear (Object : in out Optional_Stream_Reference) with
     Post => not Object.Exists;

   function First (Object : Reference; Position : Cursor) return Event;

   package Iteration is
      function Retrieve (Object : Reference;
                         Position : Cursor) return Stream_Reference
        with Pre => Position /= No_Element;
   end Iteration;
private
   type Anchor_Info is record
      Position : Positive;
      Has_Been_Output : Boolean;
   end record;

   package Anchor_To_Index is new Ada.Containers.Hashed_Maps
     (Text.Reference, Anchor_Info, Text.Hash, Text."=");

   type Cursor is new Anchor_To_Index.Cursor;

   type Instance is limited new Event_Holder with record
      Anchor_Map : Anchor_To_Index.Map;
      Stream_Count : Natural := 0;
      Depth : Natural := 0;
   end record;

   type Reference is new Ada.Finalization.Controlled with record
      Data : not null access Instance;
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   type Optional_Reference is new Ada.Finalization.Controlled with record
      Data : access Instance;
   end record;

   overriding procedure Adjust (Object : in out Optional_Reference);
   overriding procedure Finalize (Object : in out Optional_Reference);

   Null_Reference : constant Optional_Reference :=
     (Ada.Finalization.Controlled with Data => null);

   No_Element : constant Cursor := Cursor (Anchor_To_Index.No_Element);

   type Stream_Instance is limited new Refcount_Base with record
      Object  : Reference;
      Depth   : Natural;
      Current : Positive;
   end record;

   overriding procedure Finalize (Object : in out Stream_Instance);

   type Stream_Reference is new Ada.Finalization.Controlled with record
      Data : not null access Stream_Instance;
   end record;

   overriding procedure Adjust (Object : in out Stream_Reference);
   overriding procedure Finalize (Object : in out Stream_Reference);

   type Optional_Stream_Reference is new Ada.Finalization.Controlled with record
      Data : access Stream_Instance;
   end record;

   overriding procedure Adjust (Object : in out Optional_Stream_Reference);
   overriding procedure Finalize (Object : in out Optional_Stream_Reference);
end Yaml.Events.Store;
