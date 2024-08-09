with GNAT.Sockets;

package Speed_Daemon.Heartbeat_Manager is
   procedure Start_Sending (Socket : GNAT.Sockets.Socket_Type;
      Period_In_Milliseconds : Natural);
   procedure Stop_Sending (Socket : GNAT.Sockets.Socket_Type);
end Speed_Daemon.Heartbeat_Manager;
