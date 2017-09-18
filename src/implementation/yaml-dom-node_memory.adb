--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Dom.Node_Memory is
   procedure Visit (Object : in out Instance;
                    Value : not null access constant Node.Instance;
                    Previously_Visited : out Boolean) is
   begin
      if Object.Data.Contains (Value.all'Address) then
         Previously_Visited := True;
      else
         Object.Data.Include (Value.all'Address);
         Previously_Visited := False;
      end if;
   end Visit;

   procedure Forget (Object : in out Instance;
                     Value  : not null access constant Node.Instance) is
   begin
      Object.Data.Exclude (Value.all'Address);
   end Forget;

   procedure Visit (Object : in out Pair_Instance;
                    Left, Right : not null access constant Node.Instance;
                    Previously_Visited : out Boolean) is
      Pair : constant Address_Pair := (Left => Left.all'Address,
                                       Right => Right.all'Address);
   begin
      if Object.Data.Contains (Pair) then
         Previously_Visited := True;
      else
         Object.Data.Include (Pair);
         Previously_Visited := False;
      end if;
   end Visit;
end Yaml.Dom.Node_Memory;
