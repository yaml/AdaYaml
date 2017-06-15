with Ada.Finalization;

package Yaml.Sources is
   pragma Preelaborate;

   --  a Source is anything that provides a YAML character stream. Sources are
   --  always single-use objects; the parser takes ownership of
   --  sources and deallocates them.

   type Source is abstract new Ada.Finalization.Limited_Controlled with
     null record;
   type Source_Access is access all Source'Class;

   procedure Read_Data (S : in out Source; Buffer : out String;
                        Length : out Natural) is abstract;
end Yaml.Sources;
