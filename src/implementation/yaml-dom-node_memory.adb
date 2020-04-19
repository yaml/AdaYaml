--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Dom.Node_Memory is
   procedure Visit (Object : in out Instance;
                    Value : not null access Node.Instance;
                    Previously_Visited : out Boolean) is
   begin
      if Object.Data.Contains (Node_Pointer (Value)) then
         Previously_Visited := True;
      else
         Object.Data.Include (Node_Pointer (Value));
         Previously_Visited := False;
      end if;
   end Visit;

   procedure Forget (Object : in out Instance;
                     Value  : not null access Node.Instance) is
   begin
      Object.Data.Exclude (Node_Pointer (Value));
   end Forget;

   function Pop_First (Object : in out Instance)
                       return not null access Node.Instance is
      First : Pointer_Sets.Cursor := Object.Data.First;
   begin
      return Ptr : constant not null access Node.Instance :=
        Pointer_Sets.Element (First) do
         Object.Data.Delete (First);
      end return;
   end Pop_First;

   function Is_Empty (Object : Instance) return Boolean is
     (Object.Data.Is_Empty);

   procedure Visit (Object : in out Pair_Instance;
                    Left, Right : not null access Node.Instance;
                    Previously_Visited : out Boolean) is
      Pair : constant Pointer_Pair := (Left => Left, Right => Right);
   begin
      if Object.Data.Contains (Pair) then
         Previously_Visited := True;
      else
         Object.Data.Include (Pair);
         Previously_Visited := False;
      end if;
   end Visit;
end Yaml.Dom.Node_Memory;
