with Yaml.Dom.Node;

package body Yaml.Dom.Mapping_Data is
   function Hash (Object : Node_Pointer) return Ada.Containers.Hash_Type is
     (Node.Hash (Object.all));

   function Length (Object : Instance) return Count_Type is
     (Object.Data.Length);

   function Is_Empty (Container : Instance) return Boolean is
     (Container.Data.Is_Empty);

   procedure Clear (Container : in out Instance) is
   begin
      Container.Data.Clear;
   end Clear;

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
              Document => Document_Instance_Access (Position.Container.Document)));
   end Key;

   function Value (Position : Cursor) return Node_Reference is
   begin
      Increase_Refcount (Position.Container.Document);
      return ((Ada.Finalization.Controlled with
                Data => Node_Maps.Element (Position.Position),
              Document => Document_Instance_Access (Position.Container.Document)));
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
                                          Content => Text.Held (Holder),
                                          Scalar_Style => Any);
   begin
      Increase_Refcount (Object.Document);
      return Object.Element (Node_Reference'(Ada.Finalization.Controlled with
                               Data => As_Node'Unrestricted_Access,
                             Document => Object.Document));
   end Element;

   procedure Insert (Container : in out Instance;
                     Key       : in     Node_Reference;
                     New_Item  : in     Node_Reference;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean) is
   begin
      Position.Container := Container'Unrestricted_Access;
      Container.Data.Insert (Key.Data, New_Item.Data, Position.Position,
                             Inserted);
   end Insert;

   procedure Insert (Container : in out Instance;
                     Key       : in     Node_Reference;
                     New_Item  : in     Node_Reference) is
   begin
      Container.Data.Insert (Key.Data, New_Item.Data);
   end Insert;

   procedure Include (Container : in out Instance;
                      Key       : in     Node_Reference;
                      New_Item  : in     Node_Reference) is
   begin
      Container.Data.Include (Key.Data, New_Item.Data);
   end Include;

   procedure Replace (Container : in out Instance;
                      Key       : in     Node_Reference;
                      New_Item  : in     Node_Reference) is
   begin
      Container.Data.Replace (Key.Data, New_Item.Data);
   end Replace;

   procedure Exclude (Container : in out Instance;
                      Key       : in     Node_Reference) is
   begin
      Container.Data.Exclude (Key.Data);
   end Exclude;

   procedure Delete (Container : in out Instance;
                     Key       : in     Node_Reference) is
   begin
      Container.Data.Delete (Key.Data);
   end Delete;

   procedure Delete (Container : in out Instance;
                     Position  : in out Cursor) is
   begin
      Container.Data.Delete (Position.Position);
   end Delete;

   package body Friend_Interface is
      function For_Document (Document : not null access Document_Instance)
                             return Instance is
        (Document => Document, Data => <>);

      procedure Raw_Insert (Container  : in out Instance;
                            Key, Value : not null access Node.Instance) is
      begin
         Container.Data.Insert (Node_Pointer (Key), Node_Pointer (Value));
      end Raw_Insert;
   end Friend_Interface;
end Yaml.Dom.Mapping_Data;
