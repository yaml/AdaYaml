with Ada.Finalization;

package Yada.Sources is
   pragma Preelaborate;

   type Source is abstract new Ada.Finalization.Limited_Controlled with
     null record;
   type Source_Access is not null access all Source'Class;

   procedure Read_Data (S : in out Source; Buffer : out String;
                        Length : out Natural) is abstract;
end Yada.Sources;
