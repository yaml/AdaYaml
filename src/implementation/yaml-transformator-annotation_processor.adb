--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

--  we need to explicitly with all annotations because their package
--  initialization adds them to the annotation map.
with Yaml.Transformator.Annotation.Identity;
with Yaml.Transformator.Annotation.Concatenation;
pragma Unreferenced (Yaml.Transformator.Annotation.Concatenation);

package body Yaml.Transformator.Annotation_Processor is
   procedure Free_Array is new Ada.Unchecked_Deallocation
     (Node_Array, Node_Array_Pointer);
   procedure Free_Transformator is new Ada.Unchecked_Deallocation
     (Transformator.Instance'Class, Transformator.Pointer);

   function New_Processor (Pool : Text.Pool.Reference)
                           return Pointer is
     (new Instance'(Ada.Finalization.Limited_Controlled with Pool => Pool,
                    Context => Events.Context.Empty, others => <>));

   procedure Set_Globals (Object : in out Instance;
                          Value : Events.Store.Instance) is
   begin
      Events.Store.Copy (Value, Events.Context.Global (Object.Context).Value);
   end Set_Globals;

   procedure Append (Object : in out Instance; E : Event) is
   begin
      case E.Kind is
         when Mapping_Start | Sequence_Start | Annotation_Start =>
            Object.Depth := Object.Depth + 1;
            if Object.Count = 0 then
               Object.Current := E;
               Object.Current_Exists := True;
            else
               Object.Annotations (Object.Count).Impl.Put (E);
            end if;
         when Annotation_End | Mapping_End | Sequence_End =>
            Object.Depth := Object.Depth - 1;
            if Object.Count = 0 then
               Object.Current := E;
               Object.Current_Exists := True;
            else
               Object.Annotations (Object.Count).Impl.Put (E);
            end if;
         when  Scalar | Alias =>
            if Object.Count = 0 then
               Object.Current := E;
               Object.Current_Exists := True;
            else
               Object.Annotations (Object.Count).Impl.Put (E);
            end if;
         when Document_Start | Document_End | Stream_Start | Stream_End =>
            Object.Current := E;
            Object.Current_Exists := True;
      end case;
   end Append;

   procedure Put (Object : in out Instance; E : Event) is
      Locals : Events.Store.Accessor
        renames Events.Context.Local (Object.Context).Value;
   begin
      case E.Kind is
         when Annotation_Start =>
            Locals.Memorize (E);
            declare
               use type Annotation.Maps.Cursor;
               Pos : constant Annotation.Maps.Cursor :=
                 Annotation.Map.Find (E.Name.Value);
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
               Object.Annotations (Object.Count) :=
                 (Impl => (if Pos = Annotation.Maps.No_Element then
                                Annotation.Identity.New_Identity else
                                   Annotation.Maps.Element (Pos).all (Object.Pool, Object.Context)),
                  Depth => Object.Depth);
               Object.Annotations (Object.Count).Impl.Put (E);
            end;
         when Annotation_End =>
            Object.Annotations (Object.Count).Impl.Put (E);
         when Document_Start =>
            Locals.Clear;
            Locals.Memorize (E);
            Object.Append (E);
         when others =>
            Locals.Memorize (E);
            Object.Append (E);
      end case;
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_Exists or else
        (Object.Count > 0 and then Object.Annotations (Object.Count).Impl.Has_Next));

   function Next (Object : in out Instance) return Event is
   begin
      if Object.Current_Exists then
         Object.Current_Exists := False;
         return Object.Current;
      elsif Object.Count > 0 and then Object.Annotations (Object.Count).Impl.Has_Next then
         return E : constant Event := Object.Annotations (Object.Count).Impl.Next do
            if Object.Depth = Object.Annotations (Object.Count).Depth and then
              E.Kind in Mapping_End | Sequence_End | Scalar | Alias then
               Free_Transformator (Object.Annotations (Object.Count).Impl);
               Object.Count := Object.Count - 1;
            end if;
         end return;
      else
         raise Constraint_Error with "no event to retrieve";
      end if;
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
