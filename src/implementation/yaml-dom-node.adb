--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

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
   begin
      if Object.Kind = Scalar then
         return Object.Content.Hash;
      else
         declare
            Ret    : Ada.Containers.Hash_Type := 0;
            Memory : Node_Memory.Instance;

            procedure Visit (Cur : not null access Node.Instance);
            procedure Visit_Pair (Key, Value : not null access Node.Instance);

            procedure Visit (Cur : not null access Node.Instance) is
               Visited : Boolean;
            begin
               Memory.Visit (Cur, Visited);
               if Visited then
                  return;
               end if;
               case Cur.Kind is
                  when Scalar => Ret := Ret xor Cur.Content.Hash;
                  when Sequence =>
                     Ret := Ret xor Ada.Containers.Hash_Type (Cur.Items.Length);
                     Cur.Items.Iterate (Visit'Access);
                  when Mapping =>
                     Ret := Ret xor Ada.Containers.Hash_Type (Cur.Pairs.Length);
                     Cur.Pairs.Iterate (Visit_Pair'Access);
               end case;
               Memory.Forget (Cur);
            end Visit;

            procedure Visit_Pair (Key, Value : not null access Node.Instance) is
            begin
               Visit (Key);
               Visit (Value);
            end Visit_Pair;
         begin
            if Object.Kind = Sequence then
               Object.Items.Iterate (Visit'Access);
            else
               Object.Pairs.Iterate (Visit_Pair'Access);
            end if;
            return Ret;
         end;
      end if;
   end Hash;
end Yaml.Dom.Node;
