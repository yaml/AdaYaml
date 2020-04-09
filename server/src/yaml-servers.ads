with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
with GNAT.Sockets.Server;

package Yaml.Servers is
   package HTTP renames GNAT.Sockets.Connection_State_Machine.HTTP_Server;
   package Server renames GNAT.Sockets.Server;

   type Yaml_Factory (Request_Length  : Positive;
                      Input_Size      : Server.Buffer_Length;
                      Output_Size     : Server.Buffer_Length;
                      Max_Connections : Positive) is
     new Server.Connections_Factory with null record;

   type Yaml_Client is new HTTP.HTTP_Client with null record;
   pragma Warnings (Off, "formal parameter ""From"" is not referenced");
   overriding function Create (Factory  : access Yaml_Factory;
                               Listener : access Server.Connections_Server'Class;
                               From     : GNAT.Sockets.Sock_Addr_Type)
                               return Server.Connection_Ptr is
     (new Yaml_Client (Listener       => Listener.all'Unchecked_Access,
                       Request_Length => Factory.Request_Length,
                       Input_Size     => Factory.Input_Size,
                       Output_Size    => Factory.Output_Size));

   pragma Warnings (On, "formal parameter ""From"" is not referenced");

   overriding procedure Do_Get (Client : in out Yaml_Client);
end Yaml.Servers;
