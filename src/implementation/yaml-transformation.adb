--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Transformation is
   procedure Adjust (Object : in out Reference) is
   begin
      Increase_Refcount (Object.Data);
   end Adjust;

   procedure Finalize (Object : in out Reference) is
   begin
      Decrease_Refcount (Object.Data);
   end Finalize;

   procedure Finalize (Object : in out Instance) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Transformator.Instance'Class, Transformator.Pointer);
   begin
      for Element of Object.Transformators loop
         declare
            Ptr : Transformator.Pointer := Element;
         begin
            Free (Ptr);
         end;
      end loop;
   end Finalize;

   function Transform (Original : Stream_Impl.Reference) return Instance is
   begin
      return (Refcount_Base with Original => Original,
              Transformators => <>);
   end Transform;

   function Transform (Original : Stream_Impl.Reference) return Reference is
      Ptr : constant not null Instance_Access :=
        new Instance'(Refcount_Base with Original => Original,
                      Transformators => <>);
   begin
      return (Ada.Finalization.Controlled with Data => Ptr);
   end Transform;

   function Value (Object : Reference) return Accessor is
     ((Data => Object.Data.all'Access));

   function Next (Object : in out Instance) return Event is
      use type Transformator_Vectors.Cursor;

      Cursor : Transformator_Vectors.Cursor := Object.Transformators.Last;
      Current : Event;
   begin
      Outer : loop
         while Cursor /= Transformator_Vectors.No_Element loop
            exit when Transformator_Vectors.Element (Cursor).Has_Next;
            Transformator_Vectors.Previous (Cursor);
         end loop;
         if Cursor = Transformator_Vectors.No_Element then
            Current :=
              Stream_Impl.Next (Stream_Impl.Value (Object.Original).Data.all);
            Cursor := Object.Transformators.First;
         else
            Current := Transformator_Vectors.Element (Cursor).Next;
            Transformator_Vectors.Next (Cursor);
         end if;
         loop
            exit Outer when Cursor = Transformator_Vectors.No_Element;
            Transformator_Vectors.Element (Cursor).Put (Current);
            exit when not Transformator_Vectors.Element (Cursor).Has_Next;
            Current := Transformator_Vectors.Element (Cursor).Next;
            Transformator_Vectors.Next (Cursor);
         end loop;
         Transformator_Vectors.Previous (Cursor);
      end loop Outer;
      return Current;
   end Next;

   procedure Append (Object : in out Instance;
                     T : not null Transformator.Pointer) is
   begin
      Object.Transformators.Append (T);
   end Append;
end Yaml.Transformation;
