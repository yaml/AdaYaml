--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with System;
with Yaml.C;

package Lexer.Source.C_Handler is
   type Instance is new Source.Instance with private;

   overriding procedure Read_Data (S : in out Instance; Buffer : out String;
                                   Length : out Natural);

   function As_Source (Data : System.Address; Handler : Yaml.C.Read_Handler)
                       return Pointer;
private
   type Instance is new Source.Instance with record
      Handler : Yaml.C.Read_Handler;
      Data : System.Address;
   end record;
end Lexer.Source.C_Handler;
