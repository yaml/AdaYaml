--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Node;

package body Yaml.Dom.Sequence_Data is
   use type Count_Type;

   type Iterator is new Iterators.Forward_Iterator with record
      Container : not null access constant Instance;
   end record;

   function First (Object : Iterator) return Cursor is
     ((Container => Object.Container, Index => 1));

   function Next (Object : Iterator; Position : Cursor)
                  return Cursor is
     ((Container => Object.Container,
       Index => Natural'Min (Position.Index,
                             Object.Container.Data.Last_Index) + 1));

   function "=" (Left, Right : Instance) return Boolean is
   begin
      if Left.Data.Length /= Right.Data.Length then
         return False;
      else
         for Index in 1 .. Left.Data.Length loop
            if Left.Data (Positive (Index)) /= Right.Data (Positive (Index)) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   function Capacity (Container : Instance) return Count_Type is
     (Container.Data.Capacity);

   procedure Reserve_Capacity (Container : in out Instance;
                               Capacity  : in     Count_Type) is
   begin
      Container.Data.Reserve_Capacity (Capacity);
   end Reserve_Capacity;

   function Length (Object : Instance) return Count_Type is
     (Object.Data.Length);

   function Is_Empty (Container : Instance) return Boolean is
     (Container.Data.Is_Empty);

   procedure Clear (Container : in out Instance) is
   begin
      Container.Data.Clear;
   end Clear;

   function To_Cursor (Container : Instance;
                       Index     : Natural) return Cursor is
     ((Container => Container'Unrestricted_Access, Index => Index));

   function To_Index (Position  : Cursor) return Natural is
     (Position.Index);

   function Has_Element (Position : Cursor) return Boolean is
     (Position.Container /= null and then Position.Index > 0 and then
      Position.Index <= Position.Container.Data.Last_Index);

   function Element (Object : Instance; Index : Positive)
                     return Node_Reference is
   begin
      Increase_Refcount (Object.Document);
      return ((Ada.Finalization.Controlled with
                Data => Object.Data.Element (Index),
              Document => Object.Document));
   end Element;

   function Element (Object : Instance; Position : Cursor)
                     return Node_Reference is
     (Object.Element (Positive (Position.Index)));

   procedure Iterate (Object : Instance;
                      Process : not null access procedure
                        (Item : not null access Node.Instance)) is
   begin
      for Item of Object.Data loop
         Process.all (Item);
      end loop;
   end Iterate;

   function Iterate (Object : Instance)
                     return Iterators.Forward_Iterator'Class is
     (Iterator'(Container => Object'Unrestricted_Access));

   procedure Replace_Element (Container : in out Instance;
                              Index     : in     Positive;
                              New_Item  : in     Node_Reference) is
   begin
      Container.Data.Replace_Element (Index, New_Item.Data);
   end Replace_Element;

   procedure Replace_Element (Container : in out Instance;
                              Position  : in     Cursor;
                              New_Item  : in     Node_Reference) is
   begin
      Container.Data.Replace_Element (Position.Index, New_Item.Data);
   end Replace_Element;

   procedure Insert (Container : in out Instance;
                     Before    : in     Positive;
                     New_Item  : in     Node_Reference) is
   begin
      Container.Data.Insert (Before, New_Item.Data);
   end Insert;

   procedure Insert (Container : in out Instance;
                     Before    : in     Cursor;
                     New_Item  : in     Node_Reference) is
   begin
      Container.Data.Insert (Before.Index, New_Item.Data);
   end Insert;

   procedure Insert (Container : in out Instance;
                     Before    : in     Cursor;
                     New_Item  : in     Node_Reference;
                     Position  :    out Cursor) is
      Raw_Cursor : Node_Vectors.Cursor;
   begin
      Container.Data.Insert (Container.Data.To_Cursor (Before.Index),
                             New_Item.Data, Raw_Cursor);
      Position := (Container => Container'Unrestricted_Access,
                   Index => Node_Vectors.To_Index (Raw_Cursor));
   end Insert;

   procedure Prepend (Container : in out Instance;
                      New_Item  : in     Node_Reference) is
   begin
      Container.Data.Prepend (New_Item.Data);
   end Prepend;

   procedure Append (Container : in out Instance;
                     New_Item  : in     Node_Reference) is
   begin
      Container.Data.Append (New_Item.Data);
   end Append;

   procedure Delete (Container : in out Instance;
                     Index     : in     Positive;
                     Count     : in     Count_Type := 1) is
   begin
      Container.Data.Delete (Index, Count);
   end Delete;

   procedure Delete (Container : in out Instance;
                     Position  : in out Cursor;
                     Count     : in     Count_Type := 1) is
   begin
      Container.Data.Delete (Position.Index, Count);
   end Delete;

   procedure Delete_First (Container : in out Instance;
                           Count     : in     Count_Type := 1) is
   begin
      Container.Data.Delete_First (Count);
   end Delete_First;

   procedure Delete_Last (Container : in out Instance;
                          Count     : in     Count_Type := 1) is
   begin
      Container.Data.Delete_Last (Count);
   end Delete_Last;

   procedure Reverse_Elements (Container : in out Instance) is
   begin
      Container.Data.Reverse_Elements;
   end Reverse_Elements;

   procedure Swap (Container : in out Instance;
                   I, J      : in     Positive) is
   begin
      Container.Data.Swap (I, J);
   end Swap;

   procedure Swap (Container : in out Instance;
                   I, J      : in     Cursor) is
   begin
      Container.Data.Swap (I.Index, J.Index);
   end Swap;

   function First_Index (Container : Instance) return Positive is
     (Container.Data.First_Index);

   function First (Container : Instance) return Cursor is
     ((Container => Container'Unrestricted_Access,
       Index => Container.Data.First_Index));

   function First_Element (Container : Instance) return Node_Reference is
     (Container.Element (Container.Data.First_Index));

   function Last_Index (Container : Instance) return Natural is
     (Container.Data.Last_Index);

   function Last (Container : Instance) return Cursor is
     ((Container => Container'Unrestricted_Access,
       Index => Container.Data.Last_Index));

   function Last_Element (Container : Instance) return Node_Reference is
     (Container.Element (Container.Data.Last_Index));

   function Next (Position : Cursor) return Cursor is
     ((Container => Position.Container,
       Index => Natural'Min (Position.Index,
                             Position.Container.Data.Last_Index) + 1));

   procedure Next (Position : in out Cursor) is
   begin
      Position.Index := Natural'Min (Position.Index,
                                     Position.Container.Data.Last_Index) + 1;
   end Next;

   function Previous (Position : Cursor) return Cursor is
     ((Container => Position.Container,
       Index => Position.Index - 1));

   procedure Previous (Position : in out Cursor) is
   begin
      Position.Index := Position.Index - 1;
   end Previous;

   package body Friend_Interface is
      function For_Document (Document : not null access Document_Instance)
                             return Instance is
        (Document => Document, Data => <>);

      procedure Raw_Append (Container : in out Instance;
                            New_Item  : not null access Node.Instance) is
      begin
         Container.Data.Append (Node_Pointer (New_Item));
      end Raw_Append;
   end Friend_Interface;
end Yaml.Dom.Sequence_Data;
