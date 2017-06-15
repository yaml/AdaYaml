with Ada.Finalization;

package Yaml.Destinations is

   type Destination is abstract new Ada.Finalization.Limited_Controlled with
     null record; 
   type Destination_Access is access all Destination'Class;
   
   procedure Write_Data (D : in out Destination; Buffer : String) is abstract;
end Yaml.Destinations;
