--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private with Ada.Containers.Hashed_Maps;

package Yaml.Events.Store is
   type Instance is limited new Refcount_Base with private;
   type Instance_Access is access all Instance;
   type Reference is tagged private;
   type Optional_Reference is tagged private;

   type Accessor (Data : not null access Instance) is limited null record with
     Implicit_Dereference => Data;

   type Anchor_Cursor is private;
   type Element_Cursor is private;

   function New_Store return Reference;
   function Value (Object : Reference) return Accessor;

   Null_Reference : constant Optional_Reference;

   function Value (Object : Optional_Reference) return Accessor with
     Pre => Object /= Null_Reference;

   function Optional (Object : Reference'Class) return Optional_Reference;
   function Required (Object : Optional_Reference'Class) return Reference;

   procedure Memorize (Object : in out Instance; Item : Event);
   procedure Force_Memorize (Object : in out Instance; Item : Event;
                             Position : out Element_Cursor);

   function Find (Object : Instance; Alias : Text.Reference)
                  return Anchor_Cursor;
   function Exists_In_Output (Position : Anchor_Cursor) return Boolean;
   procedure Set_Exists_In_Output (Object : in out Instance;
                                   Position : Anchor_Cursor);
   procedure Advance (Position : in out Element_Cursor);
   procedure Advance_At_Same_Level (Object : Instance;
                                    Position : in out Element_Cursor);

   No_Anchor : constant Anchor_Cursor;
   No_Element : constant Element_Cursor;

   procedure Clear (Object : in out Instance);
   procedure Copy (Source : in Instance; Target : in out Instance);

   type Stream_Instance is limited new Refcount_Base with private;
   type Stream_Instance_Access is access all Stream_Instance;

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

   function First (Object : Instance; Position : Anchor_Cursor) return Event;

   function Element (Object : Instance;
                     Position : Element_Cursor) return Event;

   function Retrieve (Object : Reference'Class;
                      Position : Anchor_Cursor) return Stream_Reference
     with Pre => Position /= No_Anchor;
   function Retrieve (Object : Reference'Class;
                      Position : Element_Cursor) return Stream_Reference
     with Pre => Position /= No_Element;

   function To_Element_Cursor (Position : Anchor_Cursor) return Element_Cursor;
private
   type Anchor_Info is record
      Position : Positive;
      Has_Been_Output : Boolean;
   end record;

   package Anchor_To_Index is new Ada.Containers.Hashed_Maps
     (Text.Reference, Anchor_Info, Text.Hash, Text."=");

   type Anchor_Cursor is new Anchor_To_Index.Cursor;
   type Element_Cursor is new Natural;
   subtype Depth_Type is Integer with Static_Predicate =>
     Depth_Type = Integer'First or Depth_Type >= 0;

   After_Annotation_End : constant Depth_Type := Integer'First;

   type Instance is limited new Event_Holder with record
      Anchor_Map : Anchor_To_Index.Map;
      Stream_Count : Natural := 0;
      Depth : Depth_Type := 0;
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

   No_Anchor : constant Anchor_Cursor :=
     Anchor_Cursor (Anchor_To_Index.No_Element);
   No_Element : constant Element_Cursor := 0;

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
