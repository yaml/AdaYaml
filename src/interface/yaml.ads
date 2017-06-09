
package Yaml is
   pragma Pure;

   type Mark is record
      Index, Line, Column : Positive;
   end record;
end Yaml;
