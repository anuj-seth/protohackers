with GNAT.Sockets;

package Select_Server is
   type Socket_Status is (Healthy, Close);
   type Data_Handler is
      access function (Socket : GNAT.Sockets.Socket_Type) return Socket_Status;
   procedure Run (Callback : Data_Handler);
end Select_Server;
