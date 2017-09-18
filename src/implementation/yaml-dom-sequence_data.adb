--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Node;

package body Yaml.Dom.Sequence_Data is
   use type Count_Type;
   use type Ada.Containers.Hash_Type;

   type Iterator is new Iterators.Forward_Iterator with record
      Container : not null access constant Instance;
   end record;

   function First (Object : Iterator) return Cursor is
     ((Container => Object.Container, Index => 1));

   function Next (Object : Iterator; Position : Cursor)
                  return Cursor is
     ((Container => Object.Container,
       Index => Count_Type'Min (Position.Index + 1,
                                Object.Container.Data.Length + 1)));

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

   function Length (Object : Instance) return Count_Type is
     (Object.Data.Length);

   function Has_Element (Position : Cursor) return Boolean is
     (Position.Container /= null and then Position.Index > 0 and then
      Position.Index <= Position.Container.Data.Length);

   function First (Object : Instance) return Cursor is
     ((Container => Object'Unrestricted_Access, Index => 1));

   function Next (Position : Cursor) return Cursor is
     ((Container => Position.Container,
       Index => Count_Type'Min (Position.Index + 1, Position.Container.Data.Length + 1)));

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
end Yaml.Dom.Sequence_Data;
