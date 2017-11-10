--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.Hash;
with System;
with Yaml.Dom.Node_Memory;

package body Yaml.Dom.Node is
   use type Text.Reference;
   use type Ada.Containers.Hash_Type;
   use type Count_Type;
   use type System.Address;

   function "=" (Left, Right : Instance) return Boolean is
      Memory : Node_Memory.Pair_Instance;

      function Equal (Left, Right : not null access Instance) return Boolean;

      function Equal (Left, Right : Sequence_Data.Instance) return Boolean is
         Cur_Left, Cur_Right : Sequence_Data.Cursor;
      begin
         if Left.Length /= Right.Length then
            return False;
         end if;
         Cur_Left := Left.First;
         Cur_Right := Right.First;
         while Sequence_Data.Has_Element (Cur_Left) loop
            if not Equal (Left.Element (Cur_Left).Value.Data,
                          Right.Element (Cur_Right).Value.Data) then
               return False;
            end if;
            Cur_Left := Sequence_Data.Next (Cur_Left);
            Cur_Right := Sequence_Data.Next (Cur_Right);
         end loop;
         return True;
      end Equal;

      function Equal (Left, Right : Mapping_Data.Instance) return Boolean is
         Cur_Left, Cur_Right : Mapping_Data.Cursor;
      begin
         if Left.Length /= Right.Length then
            return False;
         end if;
         Cur_Left := Left.First;
         while Mapping_Data.Has_Element (Cur_Left) loop
            Cur_Right := Right.Find (Mapping_Data.Key (Cur_Left));
            if not Mapping_Data.Has_Element (Cur_Right) or else
              not Equal (Mapping_Data.Value (Cur_Left).Data,
                         Mapping_Data.Value (Cur_Right).Data) then
               return False;
            end if;
            Cur_Left := Mapping_Data.Next (Cur_Left);
         end loop;
         return True;
      end Equal;

      function Equal (Left, Right : not null access Instance) return Boolean is
         Visited : Boolean;
      begin
         if Left.all'Address = Right.all'Address then
            return True;
         else
            Memory.Visit (Left, Right, Visited);
            if Visited then
               return True;
            elsif Left.Kind = Right.Kind and then Left.Tag = Right.Tag then
               case Left.Kind is
                  when Scalar => return Left.Content = Right.Content;
                  when Sequence => return Equal (Left.Items, Right.Items);
                  when Mapping => return Equal (Left.Pairs, Right.Pairs);
               end case;
            else
               return False;
            end if;
         end if;
      end Equal;
   begin
      return Equal (Left'Unrestricted_Access, Right'Unrestricted_Access);
   end "=";

   function Hash (Object : Instance) return Ada.Containers.Hash_Type is
      --  for efficiency reasons, the hash of a collection node is not
      --  calculated recursively. Instead, only the content of the node and the
      --  types and number of its children (and their content for scalars) is
      --  taken into account. this suffices as hash function as long as the
      --  objects used as keys do not get too large.
   begin
      if Object.Kind = Scalar then
         return Object.Content.Hash;
      else
         declare
            Ret : Ada.Containers.Hash_Type :=
              Ada.Strings.Hash (Object.Kind'Img);

            procedure Visit (Prefix : String;
                             Cur : not null access Node.Instance) is
            begin
               case Cur.Kind is
                  when Scalar => Ret := Ret * Cur.Content.Hash;
                  when Sequence =>
                     Ret := Ret * Ada.Strings.Hash
                       (Prefix & "Sequence" & Cur.Items.Length'Img);
                  when Mapping =>
                     Ret := Ret * Ada.Strings.Hash
                       (Prefix & "Mapping" & Cur.Pairs.Length'Img);
               end case;
            end Visit;

            procedure Visit_Item (Cur : not null access Node.Instance) is
            begin
               Visit ("Item:", Cur);
            end Visit_Item;

            procedure Visit_Pair (Key, Value : not null access Node.Instance) is
            begin
               Visit ("Key:", Key);
               Visit ("Value:", Value);
            end Visit_Pair;
         begin
            if Object.Kind = Sequence then
               Object.Items.Iterate (Visit_Item'Access);
            else
               Object.Pairs.Iterate (Visit_Pair'Access);
            end if;
            return Ret;
         end;
      end if;
   end Hash;
end Yaml.Dom.Node;
