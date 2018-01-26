--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Transformator.Annotation.Inject is
   procedure Put (Object : in out Instance; E : Event) is
   begin
      Object.State.all (Object, E);
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_Exists);

   overriding function Next (Object : in out Instance) return Event is
   begin
      return Ret : constant Event := Object.Current do
         if Object.State = Injecting_Aliased'Access then
            Object.Injecting (Object.Current_Aliased.Value.Next);
         else
            Object.Current_Exists := False;
         end if;
      end return;
   end Next;

   function New_Inject (Pool : Text.Pool.Reference;
                        Node_Context : Node_Context_Type;
                        Processor_Context : Events.Context.Reference;
                        Swallows_Previous : out Boolean)
                        return not null Pointer is
      pragma Unreferenced (Pool);
   begin
      case Node_Context is
         when Document_Root =>
            raise Annotation_Error with "@@inject: cannot inject at root node";
         when Mapping_Key =>
            raise Annotation_Error with
              "@@inject: cannot inject at mapping key; use empty key and inject at value instead";
         when Mapping_Value =>
            Swallows_Previous := True;
            return new Instance'(Ada.Finalization.Limited_Controlled with
                                 Context => Processor_Context, Depth => <>,
                                 Injecting_Mapping => True,
                                 Current_Exists => False,
                                 State => Initial'Access,
                                 Current_Aliased => <>, Current => <>);
         when Sequence_Item | Parameter_Item =>
            Swallows_Previous := False;
            return new Instance'(Ada.Finalization.Limited_Controlled with
                                 Context => Processor_Context, Depth => <>,
                                 Injecting_Mapping => False,
                                 Current_Exists => False,
                                 State => Initial'Access,
                                 Current_Aliased => <>, Current => <>);
      end case;
   end New_Inject;

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
           "@@inject does not take any parameters";
      end if;
      Object.State := After_Annotation_End'Access;
   end After_Annotation_Start;

   procedure After_Annotation_End (Object : in out Instance; E : Event) is
   begin
      Object.Depth := 1;
      case E.Kind is
         when Sequence_Start =>
            if Object.Injecting_Mapping then
               raise Annotation_Error with
                 "trying to inject a sequence into a mapping";
            end if;
         when Mapping_Start =>
            if not Object.Injecting_Mapping then
               raise Annotation_Error with
                 "trying to inject a mapping into a sequence";
            end if;
         when Alias =>
            declare
               use type Events.Context.Cursor;
               use type Text.Reference;
               Position : constant Events.Context.Cursor :=
                 Events.Context.Position (Object.Context, E.Target);
            begin
               if Position = Events.Context.No_Element then
                  raise Annotation_Error with "Unresolvable alias: *" &
                    E.Target;
               end if;
               declare
                  Stream : constant Events.Store.Stream_Reference :=
                    Events.Context.Retrieve (Position);
                  Referred : constant Event := Stream.Value.Next;
               begin
                  case Referred.Kind is
                     when Sequence_Start =>
                        if Object.Injecting_Mapping then
                           raise Annotation_Error with
                             "trying to inject a sequence into a mapping";
                        end if;
                     when Mapping_Start =>
                        if not Object.Injecting_Mapping then
                           raise Annotation_Error with
                             "trying to inject a mapping into a sequence";
                        end if;
                     when others =>
                        raise Annotation_Error with
                          "@@inject does not support this node type";
                  end case;
                  Object.Current_Aliased := Stream.Optional;
                  Object.State := Injecting_Aliased'Access;
                  Object.Injecting (Stream.Value.Next);
                  return;
               end;
            end;
         when others =>
            raise Annotation_Error with
              "@@inject does not support this node type";
      end case;
      Object.State := Injecting'Access;
   end After_Annotation_End;

   procedure Injecting (Object : in out Instance; E : Event) is
   begin
      Object.Current := E;
      case E.Kind is
         when Sequence_Start | Mapping_Start | Annotation_Start =>
            Object.Depth := Object.Depth + 1;
            return;
         when Annotation_End =>
            Object.Depth := Object.Depth - 1;
            return;
         when Sequence_End | Mapping_End =>
            Object.Depth := Object.Depth - 1;
         when others => null;
      end case;
      if Object.Depth = 0 then
         Object.State := After_Inject_End'Access;
         Object.Current_Exists := False;
      else
         Object.Current_Exists := True;
      end if;
   end Injecting;

   procedure Injecting_Aliased (Object : in out Instance; E : Event) is
      pragma Unreferenced (Object);
   begin
      raise Annotation_Error with
        "unexpected input to @@inject while injecting aliased node: " &
        E.Kind'Img;
   end Injecting_Aliased;

   procedure After_Inject_End (Object : in out Instance; E : Event) is
      pragma Unreferenced (Object);
   begin
      raise Annotation_Error with
        "unexpected input to @@inject (already finished): " & E.Kind'Img;
   end After_Inject_End;
begin
   Annotation.Map.Include ("inject", New_Inject'Access);
end Yaml.Transformator.Annotation.Inject;
