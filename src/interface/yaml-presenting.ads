with Ada.Finalization;
with Yaml.Destinations;

package Yaml.Presenting is
   type Presenter is new Ada.Finalization.Limited_Controlled with private;

private
   type Presenter is new Ada.Finalization.Limited_Controlled with record
      Dest : Destinations.Destination_Access;
   end record;
end Yaml.Presenting;
