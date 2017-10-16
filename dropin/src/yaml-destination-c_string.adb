--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Destination.C_String is
   function As_Destination (Raw : System.Address;
                            Size : Interfaces.C.size_t;
                            Size_Written : access Interfaces.C.size_t)
                            return Pointer is
      Ret : constant access Instance :=
        new Instance'(Destination.Instance with
                      Raw => Raw, Size => Integer (Size),
                      Size_Written => Size_Written);
   begin
      Ret.Size_Written.all := 0;
      return Pointer (Ret);
   end As_Destination;

   overriding procedure Write_Data (D : in out Instance; Buffer : String) is
      use type Interfaces.C.size_t;

      Dest : String (1 .. D.Size);
      for Dest'Address use D.Raw;

      New_Length : constant Integer := Integer (D.Size_Written.all) + Buffer'Length;
   begin
      if New_Length > D.Size then
         raise Destination_Error with
           "Output does not fit into destination string!";
      end if;
      Dest (Integer (D.Size_Written.all + 1) .. New_Length) := Buffer;
      D.Size_Written.all :=
        D.Size_Written.all + Interfaces.C.size_t (Buffer'Length);
   end Write_Data;
end Yaml.Destination.C_String;
