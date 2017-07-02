package body Yaml.Destinations.C_Strings is
   function As_Destination (Pointer : System.Address;
                            Size : Interfaces.C.size_t;
                            Size_Written : access Interfaces.C.size_t)
                            return Destination_Access is
      Ret : constant access C_String_Destination :=
        new C_String_Destination'(Ada.Finalization.Limited_Controlled with
                                    Pointer => Pointer, Size => Integer (Size),
                                    Size_Written => Size_Written);
   begin
      Ret.Size_Written.all := 0;
      return Destination_Access (Ret);
   end As_Destination;

   overriding procedure Write_Data (D : in out C_String_Destination;
                                    Buffer : String) is
      use type Interfaces.C.size_t;

      Dest : String (1 .. D.Size);
      for Dest'Address use D.Pointer;

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
end Yaml.Destinations.C_Strings;
