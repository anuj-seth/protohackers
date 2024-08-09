with GNAT.Sockets;

package Task_Per_Connection_Server is
   type Data_Handler is
      access procedure (Socket : GNAT.Sockets.Socket_Type);
   procedure Run (Callback : Data_Handler);
private
   Client : Data_Handler;
end Task_Per_Connection_Server;
