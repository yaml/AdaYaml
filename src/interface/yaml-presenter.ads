--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;
with Yaml.Destination;
with Yaml.Stream;
with Yaml.Stacks;

package Yaml.Presenter is
   type Instance is tagged limited private;
   type Buffer_Type is access all String;

   procedure Set_Output (P : in out Instance;
                         D : not null Destination.Pointer);
   
   procedure Set_Output (P : in out Instance;
                         Buffer : not null Buffer_Type);
   
   procedure Put (P : in out Instance; E : Event);
   
   procedure Put (P : in out Instance;
                  S : in out Stream.Reference'Class);
   
   procedure Flush (P : in out Instance);
private
   type Position_Type is
     (Before_Stream_Start, After_Stream_End, Before_Doc_Start,
      After_Directives_End, After_Implicit_Doc_Start, Before_Doc_End,
      After_Implicit_Doc_End, After_Implicit_Map_Start, After_Map_Header,
      After_Flow_Map_Start, After_Implicit_Block_Map_Key,
      After_Explicit_Block_Map_Key, After_Block_Map_Value, After_Seq_Header,
      After_Implicit_Seq_Start, After_Flow_Seq_Start,
      After_Block_Seq_Item, After_Implicit_Flow_Map_Key,
      After_Explicit_Flow_Map_Key, After_Flow_Map_Value, After_Flow_Seq_Item,
      After_Annotation_Name, After_Annotation_Param);
   
   type Level is record
      Position : Position_Type;
      Indentation : Integer;
   end record;
   
   package Level_Stacks is new Yaml.Stacks (Level);
   
   type Instance is new Ada.Finalization.Limited_Controlled with record
      Cur_Column : Positive;
      Buffer_Pos : Positive;
      Buffer : Buffer_Type;
      Dest : Destination.Pointer;
      Levels : Level_Stacks.Stack;
   end record;
   
   overriding procedure Finalize (Object : in out Instance);
end Yaml.Presenter;
