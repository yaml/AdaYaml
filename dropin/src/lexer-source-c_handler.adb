--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Interfaces.C;

package body Lexer.Source.C_Handler is
   use type Yaml.C.Bool;

   procedure Read_Data (S : in out Instance; Buffer : out String;
                        Length : out Natural) is
   begin
      if not S.Handler.all (S.Data, Buffer (Buffer'First)'Address,
                            Buffer'Length, Interfaces.C.size_t (Length)) then
         raise Lexer_Error with "Error when reading into buffer";
      end if;
   end Read_Data;

   function As_Source (Data : System.Address; Handler : Yaml.C.Read_Handler)
                       return Pointer is
     (new Instance'(Source.Instance with
                    Handler => Handler, Data => Data));
end Lexer.Source.C_Handler;
