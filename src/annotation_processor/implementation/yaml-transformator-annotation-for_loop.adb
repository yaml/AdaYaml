--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Transformator.Annotation.For_Loop is
   use type Events.Context.Cursor;

   procedure Put (Object : in out Instance; E : Event) is
   begin
      Object.State.all (Object, E);
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.State = Emitting'Access and
        Object.Emitting_State /= Emitting_Finished);

   procedure Start_Next_Loop_Iteration (Object : in out Instance) is
   begin
      Object.Context.Update_Symbol
        (Object.Header_Locals, Object.Loop_Variable, Object.Context.To_Cursor
           (Object.Loop_Variable_Store, Object.Loop_Variable_Target));
      Object.Next_Event := Object.Body_Start;
   end Start_Next_Loop_Iteration;

   function Next (Object : in out Instance) return Event is
   begin
      if Object.State /= Emitting'Access then
         raise Constraint_Error with "no event available";
      end if;
      case Object.Emitting_State is
         when Emit_Sequence_Start =>
            Object.Emitting_State := Emit_Sequence_Body_Start;
            return (Kind => Sequence_Start, Start_Position => <>,
                    End_Position => <>, Collection_Style => <>,
                    Collection_Properties => Object.Node_Properties);
         when Emit_Sequence_Body_Start =>
            Start_Next_Loop_Iteration (Object);
            Object.Emitting_State := Emit_Sequence_Body;
         when Emit_Sequence_Body =>
            null;
         when Emit_Sequence_End =>
            Object.Emitting_State := Emitting_Finished;
            return (Kind => Sequence_End, Start_Position => <>,
                    End_Position => <>);
         when others =>
            raise Constraint_Error with "no event available";
      end case;
      return Ret : constant Event :=
        Object.Body_Store.Value.Element (Object.Next_Event) do
         case Ret.Kind is
            when Annotation_Start | Mapping_Start | Sequence_Start =>
               Object.Depth := Object.Depth + 1;
            when Annotation_End =>
               Object.Depth := Object.Depth - 1;
               goto After_Depth_Check;
            when Sequence_End | Mapping_End =>
               Object.Depth := Object.Depth - 1;
            when Scalar | Alias => null;
            when Stream_Start | Stream_End | Document_Start |
                 Document_End =>
               raise Stream_Error with "Unexpected event: " &
                 Ret.Kind'Img;
         end case;

         if Object.Depth = 0 then
            Object.Loop_Variable_Store.Value.Advance_At_Same_Level
              (Object.Loop_Variable_Target);
            if Object.Loop_Variable_Store.Value.Element
              (Object.Loop_Variable_Target).Kind = Sequence_End then
               Object.Emitting_State := Emit_Sequence_End;
               return;
            else
               Object.Emitting_State := Emit_Sequence_Body_Start;
               return;
            end if;
         end if;
         <<After_Depth_Check>>
         Events.Store.Advance (Object.Next_Event);
      end return;
   end Next;

   function New_For_Loop (Pool : Text.Pool.Reference;
                          Node_Context : Node_Context_Type;
                          Processor_Context : Events.Context.Reference;
                          Swallows_Previous : out Boolean)
                          return not null Pointer is
      pragma Unreferenced (Pool);
      pragma Unreferenced (Node_Context);
   begin
      Swallows_Previous := False;
      return new Instance'(Ada.Finalization.Limited_Controlled with
                             Context => Processor_Context,
                           Node_Properties => <>, State => Initial'Access,
                           Depth => 0, Body_Store => <>, Loop_Variable => <>,
                           Loop_Variable_Store => <>,
                           Loop_Variable_Target => <>, Body_Start => <>,
                           Emitting_State => Emit_Sequence_Start,
                           Next_Event => <>,
                           Header_Locals => Events.Context.No_Local_Store,
                           Body_Locals => Events.Context.No_Local_Store);
   end New_For_Loop;

   procedure Initial (Object : in out Instance; E : Event) is
   begin
      if E.Kind /= Annotation_Start then
         raise Stream_Error with
           "unexpected token (expected annotation start): " & E.Kind'Img;
      end if;
      Object.Node_Properties := E.Annotation_Properties;
      Object.State := After_Annotation_Start'Access;
   end Initial;

   procedure After_Annotation_Start (Object : in out Instance; E : Event) is
      Loop_Var_Name : Text.Reference;
   begin
      case E.Kind is
         when Scalar => Loop_Var_Name := E.Content;
         when Alias =>
            declare
               Pos : constant Events.Context.Cursor :=
                 Object.Context.Position (E.Target);
            begin
               if Pos = Events.Context.No_Element then
                  raise Annotation_Error with
                    "@@for: unresolvable alias for loop variable name";
               end if;
               declare
                  Referenced : constant Event := Events.Context.First (Pos);
               begin
                  if Referenced.Kind /= Scalar then
                     raise Annotation_Error with
                       "@@for: alias for loop variable name does not point to a scalar";
                  end if;
                  Loop_Var_Name := Referenced.Content;
               end;
            end;
         when others =>
            raise Annotation_Error with
              "@@for: loop variable name must be a scalar (got a " &
              E.Kind'Img & ")";
      end case;
      Object.Context.Create_Local_Symbol_Scope (Object.Header_Locals);
      Object.Context.Create_Symbol
        (Object.Header_Locals, Loop_Var_Name, Object.Loop_Variable);
      Object.State := After_Variable_Name'Access;
   end After_Annotation_Start;

   procedure After_Variable_Name (Object : in out Instance; E : Event) is
   begin
      case E.Kind is
         when Sequence_Start =>
            Object.Loop_Variable_Store := Object.Context.Local_Store_Ref
              (Object.Header_Locals);
            Object.Loop_Variable_Store.Value.Force_Memorize
              (E, Object.Loop_Variable_Target);
            Object.Depth := 1;
            Object.State := In_Sequence_Parameter'Access;
         when Alias =>
            Events.Context.Get_Store_And_Cursor
              (Object.Context.Position (E.Target), Object.Loop_Variable_Store,
               Object.Loop_Variable_Target);
            Object.State := After_Sequence_Parameter'Access;
         when others =>
            raise Annotation_Error with
              "@@for: expected sequence to loop over, got " & E.Kind'Img;
      end case;
   end After_Variable_Name;

   procedure In_Sequence_Parameter (Object : in out Instance; E : Event) is
   begin
      Object.Loop_Variable_Store.Value.Memorize (E);
      case E.Kind is
         when Sequence_Start | Mapping_Start | Annotation_Start =>
            Object.Depth := Object.Depth + 1;
         when Mapping_End | Annotation_End =>
            Object.Depth := Object.Depth - 1;
            if Object.Depth = 0 then
               raise Stream_Error with "illegal event (expected sequence end): "
                 & E.Kind'Img;
            end if;
         when Sequence_End =>
            Object.Depth := Object.Depth - 1;
            if Object.Depth = 0 then
               Object.State := After_Sequence_Parameter'Access;
            end if;
         when others => null;
      end case;
   end In_Sequence_Parameter;

   procedure After_Sequence_Parameter (Object : in out Instance; E : Event) is
   begin
      if E.Kind /= Annotation_End then
         raise Annotation_Error with "@@for: got more parameter than expected";
      end if;
      Events.Store.Advance (Object.Loop_Variable_Target);
      Object.State := After_Annotation_End'Access;
   end After_Sequence_Parameter;

   procedure After_Annotation_End (Object : in out Instance; E : Event) is
      use type Text.Reference;
   begin
      case E.Kind is
         when Alias =>
            if E.Target /= Events.Context.Symbol_Name (Object.Loop_Variable)
            then
               Events.Context.Get_Store_And_Cursor
                 (Object.Context.Position (E.Target), Object.Body_Store,
                  Object.Body_Start);
            else
               Object.Context.Create_Local_Store (Object.Body_Locals);
               Object.Body_Store := Object.Context.Local_Store_Ref
                 (Object.Body_Locals);
               Object.Body_Store.Value.Force_Memorize (E, Object.Body_Start);
            end if;
            Object.State := Emitting'Access;
         when Scalar =>
            Object.Context.Create_Local_Store (Object.Body_Locals);
            Object.Body_Store := Object.Context.Local_Store_Ref
              (Object.Body_Locals);
            Object.Body_Store.Value.Force_Memorize (E, Object.Body_Start);
            Object.State := Emitting'Access;
         when Mapping_Start | Sequence_Start | Annotation_Start =>
            Object.Context.Create_Local_Store (Object.Body_Locals);
            Object.Body_Store := Object.Context.Local_Store_Ref
              (Object.Body_Locals);
            Object.Body_Store.Value.Force_Memorize (E, Object.Body_Start);
            Object.Depth := 1;
            Object.State := In_Body'Access;
         when Sequence_End | Mapping_End | Annotation_End =>
            raise Stream_Error with "unexpected event (expected node start): " &
              E.Kind'Img;
         when Document_Start | Document_End | Stream_Start | Stream_End =>
            raise Stream_Error with "illegal event inside document: " &
              E.Kind'Img;
      end case;
   end After_Annotation_End;

   procedure In_Body (Object : in out Instance; E : Event) is
   begin
      Object.Body_Store.Value.Memorize (E);
      case E.Kind is
         when Sequence_Start | Mapping_Start | Annotation_Start =>
            Object.Depth := Object.Depth + 1;
         when Sequence_End | Mapping_End =>
            Object.Depth := Object.Depth - 1;
            if Object.Depth = 0 then
               Object.State := Emitting'Access;
            end if;
         when Annotation_End =>
            Object.Depth := Object.Depth - 1;
         when Scalar | Alias =>
            if Object.Depth = 0 then
               Object.State := Emitting'Access;
            end if;
         when Document_Start | Document_End | Stream_Start | Stream_End =>
            raise Stream_Error with "illegal event inside document: " &
              E.Kind'Img;
      end case;
   end In_Body;

   procedure Emitting (Object : in out Instance; E : Event) is
      pragma Unreferenced (Object);
   begin
      raise Annotation_Error with
        "unexpected input to @@for (already emitting): " & E.Kind'Img;
   end Emitting;

   overriding procedure Finalize (Object : in out Instance) is
      use type Events.Context.Local_Scope_Cursor;
   begin
      if Object.Header_Locals /= Events.Context.No_Local_Store then
         Object.Context.Release_Local_Store (Object.Header_Locals);
      end if;
      if Object.Body_Locals /= Events.Context.No_Local_Store then
         Object.Context.Release_Local_Store (Object.Body_Locals);
      end if;
   end Finalize;
begin
   Map.Include ("for", New_For_Loop'Access);
end Yaml.Transformator.Annotation.For_Loop;
