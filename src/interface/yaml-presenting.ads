--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;
with Yaml.Destinations;
with Yaml.Events;
with Yaml.Streams;
with Yaml.Stacks;

package Yaml.Presenting is
   type Presenter is tagged limited private;
   type Buffer_Type is access all String;

   procedure Set_Output (P : in out Presenter;
                         D : not null Destinations.Destination_Access);
   
   procedure Set_Output (P : in out Presenter;
                         Buffer : not null Buffer_Type);
   
   procedure Put (P : in out Presenter;
                  E : Events.Event);
   
   procedure Put (P : in out Presenter;
                  S : in out Streams.Event_Stream'Class);
   
   procedure Flush (P : in out Presenter);
private
   type Position_Type is
     (Before_Stream_Start, After_Stream_End, Before_Doc_Start,
      After_Directives_End, After_Implicit_Doc_Start, Before_Doc_End,
      After_Implicit_Doc_End, After_Implicit_Map_Start, After_Map_Header,
      After_Flow_Map_Start, After_Implicit_Block_Map_Key,
      After_Explicit_Block_Map_Key, After_Block_Map_Value, After_Seq_Header,
      After_Implicit_Seq_Start, After_Flow_Seq_Start,
      After_Block_Seq_Item, After_Implicit_Flow_Map_Key,
      After_Explicit_Flow_Map_Key, After_Flow_Map_Value, After_Flow_Seq_Item);
   
   type Level is record
      Position : Position_Type;
      Indentation : Integer;
   end record;
   
   package Level_Stacks is new Yaml.Stacks (Level);
   
   type Presenter is new Ada.Finalization.Limited_Controlled with record
      Cur_Column : Positive;
      Buffer_Pos : Positive;
      Buffer : Buffer_Type;
      Dest : Destinations.Destination_Access;
      Levels : Level_Stacks.Stack;
   end record;
   
   overriding procedure Finalize (Object : in out Presenter);
end Yaml.Presenting;
