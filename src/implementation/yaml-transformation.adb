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
      type Nullable is access all Transformator.Instance'Class;

      procedure Free is new Ada.Unchecked_Deallocation
        (Transformator.Instance'Class, Nullable);
   begin
      for Element of Object.Transformators loop
         declare
            Ptr : Nullable := Nullable (Element);
         begin
            Free (Ptr);
         end;
      end loop;
      Decrease_Refcount (Object.Original);
   end Finalize;

   function Transform (Original : not null access Stream_Impl.Instance)
                       return Instance is
   begin
      Increase_Refcount (Original);
      return (Stream_Base with
                Original => Original, Transformators => <>);
   end Transform;

   function Transform (Original : not null access Stream_Impl.Instance)
                       return Reference is
      Ptr : constant not null access Instance :=
        new Instance'(Stream_Base with
                         Original => Original, Transformators => <>);
   begin
      Increase_Refcount (Original);
      return (Ada.Finalization.Controlled with Data => Ptr);
   end Transform;

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
            Current := Stream_Impl.Next (Object.Original.all);
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

   procedure Append (Object : in out Instance; T : Transformator.Pointer) is
   begin
      Object.Transformators.Append (T);
   end Append;
end Yaml.Transformation;
