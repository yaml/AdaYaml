with Ada.Text_IO;
with Yaml.Servers;
with GNAT.Sockets.Server;

procedure Yaml.Server is
   Factory : aliased Servers.Yaml_Factory (200, 80, 8192, 100);
   Server  : Servers.Server.Connections_Server (Factory'Access, 8088);
   pragma Unreferenced (Server);
begin
   Factory.Trace_On (GNAT.Sockets.Server.Trace_Decoded,
                     GNAT.Sockets.Server.Trace_Decoded);
   Ada.Text_IO.Put_Line ("HTTP server started.");
   loop
      delay 60.0;
   end loop;
end Yaml.Server;
