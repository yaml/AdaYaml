--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Containers.Hashed_Maps;

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Yaml.Dom.Node;
with Yaml.Events.Queue.Stream;
with Yaml.Presenter;

package body Yaml.Dom.Dumping is
   package Conversions is new System.Address_To_Access_Conversions
     (Node.Instance);

   function Hash (Value : System.Storage_Elements.Integer_Address) return
     Ada.Containers.Hash_Type is
       (Ada.Containers.Hash_Type'Mod
          (System.Storage_Elements.Integer_Address'Pos (Value)));

   package Address_Maps is new Ada.Containers.Hashed_Maps
     (System.Storage_Elements.Integer_Address, Events.Queue.Mark, Hash,
      System.Storage_Elements."=", Events.Queue."=");

   procedure Dump_One (Document : Document_Reference;
                       Queue : not null access Events.Queue.Instance) is
      Event_Map : Address_Maps.Map;
      Anchor_Index : Natural := 0;

      function To_Anchor (Index : Natural) return String is
        ((if Index < 26 then "" else To_Anchor (Index / 26)) &
           Character'Val (Character'Pos ('a') + Index mod 26));

      procedure Visit_Pairs (Key, Value : not null access Node.Instance);

      procedure Visit (Cur : not null access Node.Instance) is
         Image : constant System.Storage_Elements.Integer_Address :=
           System.Storage_Elements.To_Integer
             (Conversions.To_Address (Cur.all'Unrestricted_Access));
         Position : constant Address_Maps.Cursor :=
           Event_Map.Find (Image);
         New_Position : Events.Queue.Mark;
      begin
         if Address_Maps.Has_Element (Position) then
            declare
               Accessor : constant Events.Queue.Element_Accessor :=
                 Queue.Element (Address_Maps.Element (Position));

               function Get_Anchor (Props : in out Properties)
                                          return Text.Reference is
               begin
                  if Props.Anchor.Length = 0 then
                     Props.Anchor := Document.Data.Pool.From_String
                       (To_Anchor (Anchor_Index));
                     Anchor_Index := Anchor_Index + 1;
                  end if;
                  return Props.Anchor;
               end Get_Anchor;

               Element_Anchor : constant Text.Reference :=
                 (if Accessor.Kind = Scalar then
                     Get_Anchor (Accessor.Scalar_Properties) else
                       Get_Anchor (Accessor.Collection_Properties));
            begin
               Queue.Append ((Kind => Alias, Target => Element_Anchor,
                              Start_Position => <>, End_Position => <>));
            end;
            return;
         end if;
         case Cur.Kind is
            when Scalar =>
               Queue.Append ((Kind => Scalar, Scalar_Properties =>
                                (Tag => Cur.Tag, Anchor => <>),
                              Content => Cur.Content,
                              Start_Position => <>, End_Position => <>,
                              Scalar_Style => Cur.Scalar_Style),
                             New_Position);
               Event_Map.Insert (Image, New_Position);
            when Mapping =>
               Queue.Append ((Kind => Mapping_Start,
                              Collection_Properties => (Tag => Cur.Tag,
                                                        Anchor => <>),
                              Collection_Style => Cur.Mapping_Style,
                              Start_Position => <>, End_Position => <>),
                             New_Position);
               Event_Map.Insert (Image, New_Position);
               Cur.Pairs.Iterate (Visit_Pairs'Access);
               Queue.Append ((Kind => Mapping_End, Start_Position => <>,
                              End_Position => <>));
            when Sequence =>
               Queue.Append ((Kind => Sequence_Start,
                              Collection_Properties => (Tag => Cur.Tag,
                                                        Anchor => <>),
                              Collection_Style => Cur.Sequence_Style,
                              Start_Position => <>, End_Position => <>),
                             New_Position);
               Event_Map.Insert (Image, New_Position);
               Cur.Items.Iterate (Visit'Access);
               Queue.Append ((Kind => Sequence_End, Start_Position => <>,
                              End_Position => <>));
         end case;
      end Visit;

      procedure Visit_Pairs (Key, Value : not null access Node.Instance)
      is begin
         Visit (Key);
         Visit (Value);
      end Visit_Pairs;
   begin
      Queue.Append ((Kind => Document_Start,
                     Implicit_Start => Document.Data.Implicit_Start,
                     others => <>));
      Visit (Document.Data.Root_Node);
      Queue.Append ((Kind => Document_End,
                     Implicit_End => Document.Data.Implicit_End,
                     others => <>));
   end Dump_One;

   function To_Event_Queue (Document : Document_Reference)
                            return Events.Queue.Reference is

   begin
      return Ret : constant Events.Queue.Reference := Events.Queue.New_Queue do
         declare
            Queue : constant not null access Events.Queue.Instance
              := Events.Queue.Value (Ret).Data;
         begin
            Queue.Append ((Kind => Stream_Start, others => <>));
            Dump_One (Document, Queue);
            Queue.Append ((Kind => Stream_End, others => <>));
         end;
      end return;
   end To_Event_Queue;

   function To_Event_Queue (Documents : Vectors.Vector)
                            return Events.Queue.Reference is
   begin
      return Ret : constant Events.Queue.Reference := Events.Queue.New_Queue do
         declare
            Queue : constant not null access Events.Queue.Instance
              := Events.Queue.Value (Ret).Data;
         begin
            Queue.Append ((Kind => Stream_Start, others => <>));
            for Document of Documents loop
               Dump_One (Document, Queue);
            end loop;
            Queue.Append ((Kind => Stream_End, others => <>));
         end;
      end return;
   end To_Event_Queue;

   procedure Consume_Queue is new Presenter.Consume (Events.Queue.Stream);

   procedure Dump (Document : Document_Reference;
                   Output : not null Destination.Pointer) is
      Queue : constant Events.Queue.Reference := To_Event_Queue (Document);
      Writer : Presenter.Instance;
   begin
      Writer.Set_Output (Output);
      Consume_Queue (Writer, Queue.As_Stream.Value);
   end Dump;

   procedure Dump (Documents : Vectors.Vector;
                   Output : not null Destination.Pointer) is
      Queue : constant Events.Queue.Reference := To_Event_Queue (Documents);
      Writer : Presenter.Instance;
   begin
      Writer.Set_Output (Output);
      Consume_Queue (Writer, Queue.As_Stream.Value);
   end Dump;
end Yaml.Dom.Dumping;
