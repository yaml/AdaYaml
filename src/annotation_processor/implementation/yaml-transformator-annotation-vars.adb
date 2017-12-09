--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Events.Context;

package body Yaml.Transformator.Annotation.Vars is
   procedure Put (Object : in out Instance; E : Event) is
   begin
      Object.State.all (Object, E);
   end Put;

   function Has_Next (Object : Instance) return Boolean is (False);

   function Next (Object : in out Instance) return Event is
   begin
      raise Constraint_Error with "no event available";
      return (others => <>);
   end Next;

   function New_Vars (Pool : Text.Pool.Reference;
                      Node_Context : Node_Context_Type;
                      Processor_Context : Events.Context.Reference;
                      Swallows_Previous : out Boolean)
                      return not null Pointer is
      pragma Unreferenced (Pool);
   begin
      if Node_Context /= Document_Root then
         raise Annotation_Error with
           "@@vars may only be applied to a document's root node";
      end if;
      Swallows_Previous := True;
      return new Instance'(Transformator.Instance with
                             Context => Processor_Context, others => <>);
   end New_Vars;

   procedure Initial (Object : in out Instance; E : Event) is
   begin
      if E.Kind /= Annotation_Start then
         raise Stream_Error with
           "unexpected token (expected annotation start): " & E.Kind'Img;
      end if;
      Object.State := After_Annotation_Start'Access;
   end Initial;

   procedure After_Annotation_Start (Object : in out Instance; E : Event) is
   begin
      if E.Kind /= Annotation_End then
         raise Annotation_Error with
           "@@vars does not take any parameters.";
      end if;
      Object.State := After_Annotation_End'Access;
   end After_Annotation_Start;

   procedure After_Annotation_End (Object : in out Instance; E : Event) is
   begin
      if E.Kind /= Mapping_Start then
         raise Annotation_Error with
           "@@vars must be applied on a mapping.";
      end if;
      Object.State := At_Mapping_Level'Access;
   end After_Annotation_End;

   procedure At_Mapping_Level (Object : in out Instance; E : Event) is
   begin
      case E.Kind is
         when Scalar =>
            Object.Cur_Name := E.Content;
            Object.State := Inside_Value'Access;
         when Mapping_End =>
            Object.State := After_Mapping_End'Access;
         when others =>
            raise Annotation_Error with
              "mapping annotated with @@vars must only have scalar keys";
      end case;
   end At_Mapping_Level;

   procedure Inside_Value (Object : in out Instance; E : Event) is
      use type Events.Context.Location_Type;
      use type Text.Reference;
   begin
      if Object.Depth = 0 then
         declare
            Modified_Event : Event := E;
         begin
            case E.Kind is
               when Scalar =>
                  Modified_Event.Scalar_Properties.Anchor := Object.Cur_Name;
               when Mapping_Start | Sequence_Start =>
                  Modified_Event.Collection_Properties.Anchor :=
                    Object.Cur_Name;
               when Alias =>
                  declare
                     Pos : constant Events.Context.Cursor
                       := Events.Context.Position (Object.Context, E.Target);
                  begin
                     if Events.Context.Location (Pos) = Events.Context.None then
                        raise Annotation_Error with
                          "unresolvable alias: *" & E.Target;
                     end if;
                     declare
                        Referenced_Events :
                          constant Events.Store.Stream_Reference :=
                            Events.Context.Retrieve (Pos);
                        Depth : Natural := 0;
                     begin
                        Modified_Event := Referenced_Events.Value.Next;
                        case Modified_Event.Kind is
                           when Mapping_Start | Sequence_Start =>
                              Modified_Event.Collection_Properties.Anchor :=
                                Object.Cur_Name;
                           when Scalar =>
                              Modified_Event.Scalar_Properties.Anchor :=
                                Object.Cur_Name;
                           when others =>
                              raise Program_Error with
                                "alias referenced " & Modified_Event.Kind'Img;
                        end case;
                        loop
                           Object.Context.Stream_Store.Memorize
                             (Modified_Event);
                           case Modified_Event.Kind is
                           when Mapping_Start | Sequence_Start =>
                              Depth := Depth + 1;
                           when Mapping_End | Sequence_End =>
                              Depth := Depth - 1;
                           when others => null;
                           end case;
                           exit when Depth = 0;
                           Modified_Event := Referenced_Events.Value.Next;
                        end loop;
                     end;
                  end;
                  Object.State := At_Mapping_Level'Access;
                  return;
               when others =>
                  raise Stream_Error with
                    "Unexpected event (expected node start): " & E.Kind'Img;
            end case;
            Object.Cur_Queue.Append (Modified_Event);
         end;
      elsif E.Kind = Alias then
         declare
            Pos : constant Events.Context.Cursor
              := Events.Context.Position (Object.Context, E.Target);
         begin
            if Events.Context.Location (Pos) = Events.Context.None then
               raise Annotation_Error with
                 "unresolvable alias: *" & E.Target;
            end if;
            declare
               Referenced_Events : constant Events.Store.Stream_Reference :=
                 Events.Context.Retrieve (Pos);
               Depth : Natural := 0;
               Cur_Event : Event := Referenced_Events.Value.Next;
            begin
               loop
                  Object.Cur_Queue.Append (Cur_Event);
                  case Cur_Event.Kind is
                     when Mapping_Start | Sequence_Start =>
                        Depth := Depth + 1;
                     when Mapping_End | Sequence_End =>
                        Depth := Depth - 1;
                     when others => null;
                  end case;
                  exit when Depth = 0;
                  Cur_Event := Referenced_Events.Value.Next;
               end loop;
            end;
         end;
      else
         Object.Cur_Queue.Append (E);
      end if;
      case E.Kind is
         when Mapping_Start | Sequence_Start =>
            Object.Depth := Object.Depth + 1;
            return;
         when Mapping_End | Sequence_End =>
            Object.Depth := Object.Depth - 1;
         when others => null;
      end case;
      if Object.Depth = 0 then
         loop
            Object.Context.Stream_Store.Memorize
              (Object.Cur_Queue.First);
            Object.Cur_Queue.Dequeue;
            exit when Object.Cur_Queue.Length = 0;
         end loop;
         Object.State := At_Mapping_Level'Access;
      end if;
   end Inside_Value;

   procedure After_Mapping_End (Object : in out Instance; E : Event) is
   begin
      raise Constraint_Error with
        "unexpected input to @@vars (already finished)";
   end After_Mapping_End;
begin
   Map.Include ("vars", New_Vars'Access);
end Yaml.Transformator.Annotation.Vars;
