--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Destination.C_Handler is
   use type Yaml.C.Bool;

   function As_Destination (Handler : Yaml.C.Write_Handler;
                            Data : System.Address) return Pointer is
     (new Instance'(Destination.Instance with
                    Handler => Handler, Data => Data));

   procedure Write_Data (Object : in out Instance; Buffer : String) is
   begin
      if not Object.Handler.all
        (Object.Data, Buffer (Buffer'First)'Address, Buffer'Length) then
         raise Destination_Error with "Custom write handler failed";
      end if;
   end Write_Data;
end Yaml.Destination.C_Handler;
