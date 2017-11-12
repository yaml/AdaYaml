--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

--  we need to explicitly with all annotations because their package
--  initialization adds them to the annotation map.
with Yaml.Transformator.Annotation.Identity;
with Yaml.Transformator.Annotation.Concatenation;
with Yaml.Transformator.Annotation.Vars;
pragma Unreferenced (Yaml.Transformator.Annotation.Concatenation);
pragma Unreferenced (Yaml.Transformator.Annotation.Vars);

package body Yaml.Transformator.Annotation_Processor is
   use type Text.Reference;

   procedure Free_Array is new Ada.Unchecked_Deallocation
     (Node_Array, Node_Array_Pointer);
   procedure Free_Transformator is new Ada.Unchecked_Deallocation
     (Transformator.Instance'Class, Transformator.Pointer);

   function New_Processor
     (Pool : Text.Pool.Reference;
      Externals : Events.Store.Reference := Events.Store.New_Store)
      return Pointer is
     (new Instance'(Ada.Finalization.Limited_Controlled with Pool => Pool,
                    Context => Events.Context.Create (Externals),
                    others => <>));

   procedure Append (Object : in out Instance; E : Event; Start : Natural) is
   begin
      if Object.Current_State = Existing_But_Held_Back then
         Object.Held_Back := Object.Current;
         Object.Current := E;
      else
         declare
            Cur_Annotation : Natural := Start;
            Cur_Event : Event := E;
         begin
            while Cur_Annotation > 0 loop
               Object.Annotations (Cur_Annotation).Impl.Put (Cur_Event);
               if Object.Annotations (Cur_Annotation).Impl.Has_Next then
                  Cur_Event := Object.Annotations (Cur_Annotation).Impl.Next;
                  Cur_Annotation := Cur_Annotation - 1;
               else
                  loop
                     Cur_Annotation := Cur_Annotation + 1;
                     if Cur_Annotation > Object.Count then
                        return;
                     end if;
                     if Object.Annotations (Cur_Annotation).Impl.Has_Next then
                        Cur_Event :=
                          Object.Annotations (Cur_Annotation).Impl.Next;
                        Cur_Annotation := Cur_Annotation - 1;
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
            Object.Current := Cur_Event;
            Object.Current_State := Existing;
         end;
      end if;
   end Append;

   procedure Finalize_Finished_Annotation_Impl (Object : in out Instance) is
   begin
      if Object.Count > 0 and then
        Object.Depth = Object.Annotations (Object.Count).Depth and then
        not Object.Annotations (Object.Count).Impl.Has_Next then
         if Object.Annotations (Object.Count).Impl.Swallows_Document then
            Object.Current_State := Swallowing_Document_End;
            Object.Current := (Kind => Document_End, others => <>);
         end if;
         Free_Transformator
           (Object.Annotations (Object.Count).Impl);
         Object.Count := Object.Count - 1;
      end if;
   end Finalize_Finished_Annotation_Impl;

   procedure Put (Object : in out Instance; E : Event) is
      Locals : constant Events.Store.Accessor := Object.Context.Document_Store;
   begin
      case E.Kind is
         when Annotation_Start =>
            Locals.Memorize (E);
            declare
               use type Annotation.Maps.Cursor;
               Pos : constant Annotation.Maps.Cursor :=
                 (if E.Namespace = Standard_Annotation_Namespace then
                     Annotation.Map.Find (E.Name.Value) else
                       Annotation.Maps.No_Element);
            begin
               if Object.Count = Object.Annotations.all'Length then
                  declare
                     Old_Array : Node_Array_Pointer := Object.Annotations;
                  begin
                     Object.Annotations := new Node_Array (1 .. Object.Count * 2);
                     Object.Annotations (1 .. Object.Count) := Old_Array.all;
                     Free_Array (Old_Array);
                  end;
               end if;
               Object.Count := Object.Count + 1;
               declare
                  Impl : constant Transformator.Pointer :=
                    (if Pos = Annotation.Maps.No_Element then
                                Annotation.Identity.New_Identity else
                                   Annotation.Maps.Element (Pos).all
                         (Object.Pool, Object.Context));
               begin
                  Object.Annotations (Object.Count) :=
                    (Impl => Impl, Depth => Object.Depth);
                  Impl.Put (E);
                  if Impl.Swallows_Document then
                     if Object.Current_State /= Existing_But_Held_Back then
                        raise Annotation_Error with
                        E.Namespace & E.Name &
                          " may only be applied on the root node of a document";
                     end if;
                     Object.Current_State := Absent;
                  elsif Impl.Has_Next then
                     Object.Append (Impl.Next, Object.Count - 1);
                  elsif Object.Current_State = Existing_But_Held_Back then
                     Object.Current_State := Existing;
                  end if;
               end;
            end;
         when Annotation_End =>
            Locals.Memorize (E);
            Object.Append (E, Object.Count);
         when Document_Start =>
            Locals.Clear;
            Object.Current := E;
            Object.Current_State := Existing_But_Held_Back;
         when Mapping_Start | Sequence_Start =>
            Object.Depth := Object.Depth + 1;
            Locals.Memorize (E);
            Object.Append (E, Object.Count);
         when Mapping_End | Sequence_End =>
            Object.Depth := Object.Depth - 1;
            Locals.Memorize (E);
            Object.Append (E, Object.Count);
            Finalize_Finished_Annotation_Impl (Object);
         when Document_End =>
            if Object.Current_State = Swallowing_Document_End then
               Object.Current_State := Absent;
            else
               Object.Current := E;
               Object.Current_State := Existing;
            end if;
         when Scalar | Alias =>
            Locals.Memorize (E);
            Object.Append (E, Object.Count);
            Finalize_Finished_Annotation_Impl (Object);
         when others =>
            Object.Append (E, Object.Count);
      end case;
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_State = Existing or
        (Object.Current_State = Existing_But_Held_Back and
             Object.Current.Kind /= Document_Start) or
        (Object.Current_State = Swallowing_Document_End and
             Object.Current.Kind /= Document_End));

   function Next (Object : in out Instance) return Event is
      use type Events.Context.Location_Type;

      procedure Look_For_Additional_Element is
      begin
         if Object.Count > 0 and then
           Object.Annotations (1).Impl.Has_Next
         then
            Object.Current := Object.Annotations (1).Impl.Next;
            if Object.Count = 1 then
               Finalize_Finished_Annotation_Impl (Object);
            end if;
         else
            Object.Current_State := Absent;
         end if;
      end Look_For_Additional_Element;

      procedure Update_Exists_In_Output (Anchor : Text.Reference) is
         use type Events.Context.Cursor;
      begin
         if Anchor /= Text.Empty then
            declare
               Pos : Events.Context.Cursor :=
                 Events.Context.Position (Object.Context, Anchor);
            begin
               if Pos /= Events.Context.No_Element then
                  declare
                     Referenced : constant Event := Events.Context.First (Pos);
                  begin
                     if
                       Referenced.Start_Position =
                         Object.Current.Start_Position and
                         Referenced.Kind = Object.Current.Kind then
                        Events.Context.Set_Exists_In_Output (Pos);
                     end if;
                  end;
               end if;
            end;
         end if;
      end Update_Exists_In_Output;
   begin
      case Object.Current_State is
         when Existing =>
            case Object.Current.Kind is
               when Alias =>
                  declare
                     Pos : Events.Context.Cursor :=
                       Events.Context.Position
                         (Object.Context, Object.Current.Target);
                  begin
                     if not Events.Context.Exists_In_Ouput (Pos) then
                        Events.Context.Set_Exists_In_Output (Pos);
                        Object.Current_Stream :=
                          Events.Context.Retrieve (Pos).Optional;
                        return Ret : constant Event :=
                          Object.Current_Stream.Value.Next do
                           case Ret.Kind is
                           when Scalar =>
                              Object.Current_Stream.Clear;
                              Look_For_Additional_Element;
                           when Mapping_Start | Sequence_Start =>
                              Object.Current_State := Localizing_Alias;
                              Object.Stream_Depth := Object.Stream_Depth + 1;
                           when others =>
                              raise Program_Error with
                                "alias refers to " & Object.Current.Kind'Img;
                           end case;
                        end return;
                     end if;
                  end;
               when Scalar =>
                  Update_Exists_In_Output
                    (Object.Current.Scalar_Properties.Anchor);
               when Mapping_Start | Sequence_Start =>
                  Update_Exists_In_Output
                    (Object.Current.Collection_Properties.Anchor);
               when others => null;
            end case;
            return Ret : constant Event := Object.Current do
               Look_For_Additional_Element;
            end return;
         when Existing_But_Held_Back =>
            if Object.Current.Kind = Document_Start then
               raise Constraint_Error with "no event to retrieve";
            end if;
            Object.Current_State := Existing;
            return Object.Held_Back;
         when Localizing_Alias =>
            return Ret : constant Event := Object.Current_Stream.Value.Next do
               case Ret.Kind is
               when Mapping_Start | Sequence_Start =>
                  Object.Stream_Depth := Object.Stream_Depth + 1;
               when Mapping_End | Sequence_End =>
                  Object.Stream_Depth := Object.Stream_Depth - 1;
                  if Object.Stream_Depth = 0 then
                     Object.Current_Stream.Clear;
                     Look_For_Additional_Element;
                  end if;
                  when others => null;
               end case;
            end return;
         when Swallowing_Document_End =>
            if Object.Current.Kind = Document_End then
               raise Constraint_Error with "no event to retrieve";
            else
               return Ret : constant Event := Object.Current do
                  Object.Current := (Kind => Document_End, others => <>);
               end return;
            end if;
         when Absent =>
            raise Constraint_Error with "no event to retrieve";
      end case;
   end Next;

   procedure Finalize (Object : in out Instance) is
      Ptr : Node_Array_Pointer := Object.Annotations;
   begin
      for I in 1 .. Object.Count loop
         Free_Transformator (Object.Annotations (I).Impl);
      end loop;
      Free_Array (Ptr);
   end Finalize;
end Yaml.Transformator.Annotation_Processor;
