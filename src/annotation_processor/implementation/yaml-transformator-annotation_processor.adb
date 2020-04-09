--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

--  we need to explicitly with all annotations because their package
--  initialization adds them to the annotation map.
with Yaml.Transformator.Annotation.Identity;
with Yaml.Transformator.Annotation.Concatenation;
with Yaml.Transformator.Annotation.Vars;
with Yaml.Transformator.Annotation.For_Loop;
with Yaml.Transformator.Annotation.Inject;
pragma Unreferenced (Yaml.Transformator.Annotation.Concatenation);
pragma Unreferenced (Yaml.Transformator.Annotation.Vars);
pragma Unreferenced (Yaml.Transformator.Annotation.For_Loop);
pragma Unreferenced (Yaml.Transformator.Annotation.Inject);

package body Yaml.Transformator.Annotation_Processor is
   use type Text.Reference;
   use type Annotation.Node_Context_Type;

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

   procedure Finalize_Finished_Annotation_Impl (Object : in out Instance) is
   begin
      if Object.Level_Count = Object.Annotations (Object.Annotation_Count).Depth
        and then not Object.Annotations (Object.Annotation_Count).Impl.Has_Next
      then
         if Object.Annotations (Object.Annotation_Count).Swallows_Next then
            Object.Current_State := Swallowing_Document_End;
            Object.Current :=
              (Kind => Document_End, others => <>);
         end if;
         Free_Transformator (Object.Annotations
                             (Object.Annotation_Count).Impl);
         Object.Annotation_Count := Object.Annotation_Count - 1;
         if Object.Annotation_Count = 0 and Object.Next_Event_Storage = Yes then
            if Object.Current_State = Existing then
               Object.Next_Event_Storage := Finishing;
            else
               Object.Next_Event_Storage := No;
            end if;
         end if;
      end if;
   end Finalize_Finished_Annotation_Impl;

   procedure Shift_Through (Object : in out Instance; Start : Natural;
                            E : Event)
     with Pre => Start <= Object.Annotation_Count is

      Cur_Annotation : Natural := Start;
      Cur_Event : Event := E;
   begin
      while Cur_Annotation > 0 loop
         Object.Annotations (Cur_Annotation).Impl.Put (Cur_Event);
         if Object.Annotations (Cur_Annotation).Impl.Has_Next then
            Cur_Event := Object.Annotations (Cur_Annotation).Impl.Next;
            if (Cur_Annotation = Object.Annotation_Count and
                  Object.May_Finish_Transformation)
            then
               Finalize_Finished_Annotation_Impl (Object);
            end if;
            Cur_Annotation := Cur_Annotation - 1;
         else
            loop
               Cur_Annotation := Cur_Annotation + 1;
               if Cur_Annotation > Object.Annotation_Count then
                  if Object.May_Finish_Transformation then
                     Finalize_Finished_Annotation_Impl (Object);
                  end if;
                  return;
               end if;
               if Object.Annotations (Cur_Annotation).Impl.Has_Next then
                  Cur_Event :=
                    Object.Annotations (Cur_Annotation).Impl.Next;
                  if (Cur_Annotation = Object.Annotation_Count and
                        Object.May_Finish_Transformation)
                  then
                     Finalize_Finished_Annotation_Impl (Object);
                  end if;
                  Cur_Annotation := Cur_Annotation - 1;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      Object.Current := Cur_Event;
      Object.Current_State :=
        (if Object.Current_State = Event_Held_Back then
            Releasing_Held_Back else Existing);
   end Shift_Through;

   procedure Append (Object : in out Instance; E : Event) is
   begin
      if Object.Annotation_Count > 0 then
         if Object.Current_State = Event_Held_Back then
            Object.May_Finish_Transformation :=
              Object.Held_Back.Kind in
                Sequence_End | Mapping_End | Scalar | Alias;
            if E.Kind = Annotation_Start then
               if Object.Annotation_Count = 1 then
                  Object.Current := Object.Held_Back;
                  Object.Held_Back := E;
                  Object.Current_State := Releasing_Held_Back;
                  return;
               else
                  Shift_Through (Object, Object.Annotation_Count,
                                 Object.Held_Back);
               end if;
            else
               Shift_Through (Object, Object.Annotation_Count,
                              Object.Held_Back);
            end if;
            if Object.Current_State = Existing then
               Object.Held_Back := E;
               Object.Current_State := Releasing_Held_Back;
               return;
            end if;
            Object.Current_State := Absent;
         end if;
         Object.May_Finish_Transformation :=
           E.Kind in Sequence_End | Mapping_End | Scalar | Alias;
         Shift_Through (Object, Object.Annotation_Count, E);
      elsif Object.Current_State = Event_Held_Back then
         Object.Current := Object.Held_Back;
         Object.Held_Back := E;
         Object.Current_State := Releasing_Held_Back;
      else
         Object.Current := E;
         Object.Current_State := Existing;
      end if;
   end Append;

   generic
      type Element_Type is private;
      type Array_Type is array (Positive range <>) of Element_Type;
      type Pointer_Type is access Array_Type;
   procedure Grow (Target : in out not null Pointer_Type;
                   Last : in out Natural);

   procedure Grow (Target : in out not null Pointer_Type;
                   Last : in out Natural) is
      procedure Free_Array is new Ada.Unchecked_Deallocation
        (Array_Type, Pointer_Type);
   begin
      if Last = Target'Last then
         declare
            Old_Array : Pointer_Type := Target;
         begin
            Target := new Array_Type (1 .. Last * 2);
            Target (1 .. Last) := Old_Array.all;
            Free_Array (Old_Array);
         end;
      end if;
      Last := Last + 1;
   end Grow;

   procedure Grow_Levels is new Grow
     (Annotation.Node_Context_Type, Level_Array, Level_Array_Pointer);

   procedure Grow_Annotations is new Grow
     (Annotated_Node, Node_Array, Node_Array_Pointer);

   procedure Put (Object : in out Instance; E : Event) is
      Locals : constant Events.Store.Accessor := Object.Context.Document_Store;
   begin
      if Object.Level_Count > 0 then
         case Object.Levels (Object.Level_Count) is
            when Annotation.Mapping_Key =>
               Object.Levels (Object.Level_Count) := Annotation.Mapping_Value;
            when Annotation.Mapping_Value =>
               Object.Levels (Object.Level_Count) := Annotation.Mapping_Key;
            when others => null;
         end case;
      end if;
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
               Grow_Annotations (Object.Annotations, Object.Annotation_Count);
               if Object.Annotation_Count = 1 then
                  Object.Next_Event_Storage :=
                    (if E.Annotation_Properties.Anchor /= Text.Empty then
                        Searching else No);
               end if;

               declare
                  Swallows_Previous : Boolean := False;
                  Impl : constant Transformator.Pointer :=
                    (if Pos = Annotation.Maps.No_Element then
                                Annotation.Identity.New_Identity else
                                   Annotation.Maps.Element (Pos).all
                         (Object.Pool, Object.Levels (Object.Level_Count),
                          Object.Context, Swallows_Previous));
               begin
                  Object.Annotations (Object.Annotation_Count) :=
                    (Impl => Impl, Depth => Object.Level_Count,
                     Swallows_Next =>
                       Swallows_Previous and Object.Level_Count = 1);
                  if Swallows_Previous then
                     if Object.Current_State /= Event_Held_Back then
                        raise Annotation_Error with
                        E.Namespace & E.Name &
                          " applied to a value of a non-scalar mapping key";
                     end if;
                     Object.Current_State := Absent;
                  end if;
                  Object.Append (E);
               end;
            end;
            Grow_Levels (Object.Levels, Object.Level_Count);
            Object.Levels (Object.Level_Count) := Annotation.Parameter_Item;
         when Annotation_End =>
            Locals.Memorize (E);
            Object.Append (E);
            Object.Level_Count := Object.Level_Count - 1;
         when Document_Start =>
            Locals.Clear;
            Object.Held_Back := E;
            Object.Current_State := Event_Held_Back;
            Grow_Levels (Object.Levels, Object.Level_Count);
            Object.Levels (Object.Level_Count) := Annotation.Document_Root;
         when Mapping_Start =>
            Locals.Memorize (E);
            Object.Append (E);
            Grow_Levels (Object.Levels, Object.Level_Count);
            Object.Levels (Object.Level_Count) := Annotation.Mapping_Value;
         when Sequence_Start =>
            Locals.Memorize (E);
            Object.Append (E);
            Grow_Levels (Object.Levels, Object.Level_Count);
            Object.Levels (Object.Level_Count) := Annotation.Sequence_Item;
         when Mapping_End | Sequence_End =>
            Object.Level_Count := Object.Level_Count - 1;
            Locals.Memorize (E);
            Object.Append (E);
         when Document_End =>
            if Object.Current_State = Swallowing_Document_End then
               Object.Current_State := Absent;
            else
               Object.Current := E;
               Object.Current_State := Existing;
            end if;
            Object.Level_Count := Object.Level_Count - 1;
         when Scalar =>
            Locals.Memorize (E);
            if Object.Levels (Object.Level_Count) = Annotation.Mapping_Key then
               Object.Held_Back := E;
               Object.Current_State := Event_Held_Back;
            else
               Object.Append (E);
            end if;
         when Alias =>
            Locals.Memorize (E);
            Object.Append (E);
         when Stream_Start | Stream_End =>
            Object.Append (E);
      end case;
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_State in Existing | Releasing_Held_Back | Localizing_Alias or
        (Object.Current_State = Swallowing_Document_End and
             Object.Current.Kind /= Document_End));

   function Next (Object : in out Instance) return Event is

      procedure Look_For_Additional_Element is
      begin
         if Object.Current_State /= Releasing_Held_Back then
            Object.Current_State := Absent;
         end if;
         for Cur_Annotation in 1 .. Object.Annotation_Count loop
            if Object.Annotations (Cur_Annotation).Impl.Has_Next then
               declare
                  Next_Event : constant Event :=
                    Object.Annotations (Cur_Annotation).Impl.Next;
               begin
                  if Cur_Annotation = Object.Annotation_Count and
                    Object.May_Finish_Transformation then
                     Finalize_Finished_Annotation_Impl (Object);
                  end if;
                  Shift_Through (Object, Cur_Annotation - 1, Next_Event);
               end;
               exit;
            end if;
         end loop;
         if Object.Current_State = Releasing_Held_Back then
            Object.Current_State := Absent;
            if Object.Annotation_Count > 0 then
               Object.May_Finish_Transformation :=
                 Object.Held_Back.Kind in
                   Sequence_End | Mapping_End | Scalar | Alias;
               Shift_Through (Object, Object.Annotation_Count,
                              Object.Held_Back);
            else
               Object.Current := Object.Held_Back;
               Object.Current_State := Existing;
            end if;
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

      procedure Update_Next_Storage (E : Event) is
      begin
         case Object.Next_Event_Storage is
            when Searching =>
               if (case E.Kind is
                      when Annotation_Start =>
                         E.Annotation_Properties.Anchor /= Text.Empty,
                      when Mapping_Start | Sequence_Start =>
                         E.Collection_Properties.Anchor /= Text.Empty,
                      when Scalar =>
                         E.Scalar_Properties.Anchor /= Text.Empty,
                      when others => False) then
                  Object.Context.Transformed_Store.Memorize (E);
                  Object.Next_Event_Storage := Yes;
               else
                  Object.Next_Event_Storage := No;
               end if;
            when Finishing =>
               Object.Context.Transformed_Store.Memorize (E);
               Object.Next_Event_Storage := No;
            when Yes =>
               Object.Context.Transformed_Store.Memorize (E);
            when No => null;
         end case;
      end Update_Next_Storage;

   begin
      case Object.Current_State is
         when Existing | Releasing_Held_Back =>
            case Object.Current.Kind is
               when Alias =>
                  if Object.Current_State = Releasing_Held_Back then
                     raise Program_Error with
                       "internal error: alias may never generated while event is held back!";
                  end if;
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
                           Update_Next_Storage (Ret);
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
               Update_Next_Storage (Ret);
               Look_For_Additional_Element;
            end return;
         when Localizing_Alias =>
            return Ret : constant Event := Object.Current_Stream.Value.Next do
               Update_Next_Storage (Ret);
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
                  Update_Next_Storage (Ret);
                  Object.Current := (Kind => Document_End, others => <>);
               end return;
            end if;
         when Absent | Event_Held_Back =>
            raise Constraint_Error with "no event to retrieve";
      end case;
   end Next;

   procedure Finalize (Object : in out Instance) is
      Ptr : Node_Array_Pointer := Object.Annotations;
   begin
      for I in 1 .. Object.Annotation_Count loop
         Free_Transformator (Object.Annotations (I).Impl);
      end loop;
      Free_Array (Ptr);
   end Finalize;
end Yaml.Transformator.Annotation_Processor;
