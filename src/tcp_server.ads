with GNAT.Sockets;

package Tcp_Server is
   type Data_Handler is
      access procedure (Socket : GNAT.Sockets.Socket_Type);
   procedure Run (Callback : Data_Handler);
private
   Client : Data_Handler;
end Tcp_Server;
