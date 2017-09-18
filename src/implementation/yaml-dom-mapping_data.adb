with Yaml.Dom.Node;

package body Yaml.Dom.Mapping_Data is
   function Hash (Object : Node_Pointer) return Ada.Containers.Hash_Type is
     (Node.Hash (Object.all));

   function Length (Object : Instance) return Count_Type is
     (Object.Data.Length);

   function Has_Element (Position : Cursor) return Boolean is
     (Position.Container /= null and then
      Node_Maps.Has_Element (Position.Position));

   function First (Object : Instance) return Cursor is
     ((Container => Object'Unrestricted_Access,
       Position  => Object.Data.First));

   function Next (Position : Cursor) return Cursor is
     ((Container => Position.Container,
       Position  => Node_Maps.Next (Position.Position)));

   procedure Iterate (Object : Instance;
                      Process : not null access procedure
                        (Key, Value : not null access Node.Instance)) is
      Cur : Node_Maps.Cursor := Object.Data.First;
   begin
      while Node_Maps.Has_Element (Cur) loop
         Process.all (Node_Maps.Key (Cur), Node_Maps.Element (Cur));
         Cur := Node_Maps.Next (Cur);
      end loop;
   end Iterate;

   function Key (Position : Cursor) return Node_Reference is
   begin
      Increase_Refcount (Position.Container.Document);
      return ((Ada.Finalization.Controlled with
                Data => Node_Maps.Key (Position.Position),
              Document => Position.Container.Document));
   end Key;

   function Value (Position : Cursor) return Node_Reference is
   begin
      Increase_Refcount (Position.Container.Document);
      return ((Ada.Finalization.Controlled with
                Data => Node_Maps.Element (Position.Position),
              Document => Position.Container.Document));
   end Value;

   function Find (Object : Instance; Key : Node_Reference) return Cursor is
      Found : constant Node_Maps.Cursor := Object.Data.Find (Key.Value.Data);
   begin
      return (Container => Object'Unrestricted_Access, Position => Found);
   end Find;

   function Element (Object : Instance; Key : Node_Reference)
                     return Node_Reference is
      Found : constant Node_Maps.Cursor := Object.Data.Find (Key.Value.Data);
   begin
      if Node_Maps.Has_Element (Found) then
         Increase_Refcount (Object.Document);
         return (Ada.Finalization.Controlled with
                 Data => Node_Maps.Element (Found),
                 Document => Object.Document);
      else
         raise Constraint_Error with "No such key";
      end if;
   end Element;

   function Element (Object : Instance; Key : String) return Node_Reference is
      Holder : constant Text.Constant_Instance := Text.Hold (Key);
      As_Node : aliased Node.Instance := (Kind => Scalar, Tag => Tags.String,
                                          Content => Text.Held (Holder));
   begin
      Increase_Refcount (Object.Document);
      return Object.Element (Node_Reference'(Ada.Finalization.Controlled with
                               Data => As_Node'Unrestricted_Access,
                             Document => Object.Document));
   end Element;
end Yaml.Dom.Mapping_Data;
