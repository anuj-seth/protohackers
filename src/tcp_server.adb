with Ada.Text_IO;
with Ada.Exceptions;

package body Tcp_Server is
   package TIO renames Ada.Text_IO;
   package Sockets renames GNAT.Sockets;
   package Exceptions renames Ada.Exceptions;

   task type Connection_Handler is
      entry Start (Client_Socket : Sockets.Socket_Type);
   end Connection_Handler;
   task body Connection_Handler is
      Socket : Sockets.Socket_Type;
   begin
      accept Start (Client_Socket : Sockets.Socket_Type) do
         Socket := Client_Socket;
      end Start;
      Client (Socket => Socket);
      Sockets.Close_Socket (Socket);
   end Connection_Handler;

   procedure Run (Callback : Data_Handler) is
      Address  : Sockets.Sock_Addr_Type;
      Server   : Sockets.Socket_Type;
      Host_Name : constant String := Sockets.Host_Name;
      Host_Entry : constant Sockets.Host_Entry_Type :=
         Sockets.Get_Host_By_Name (Host_Name);
   begin
      Client := Callback;
      Address.Addr := Sockets.Addresses (Host_Entry,
                                         1);
      Address.Port := 5432;
      Sockets.Create_Socket (Server);
      Sockets.Set_Socket_Option (Server,
                                 Sockets.Socket_Level,
                                 (Sockets.Reuse_Address, True));
      Sockets.Bind_Socket (Server, Address);
      Sockets.Listen_Socket (Socket => Server,
                             Length => 5);
      loop
         declare
            Socket : Sockets.Socket_Type;
            Handler_Task : Connection_Handler;
         begin
            Sockets.Accept_Socket (Server, Socket, Address);
            Handler_Task.Start (Client_Socket => Socket);
         end;
      end loop;
      Sockets.Close_Socket (Server);
   exception when E : others =>
      TIO.Put_Line (Exceptions.Exception_Name (E) &
                    ": " &
                    Exceptions.Exception_Message (E));
   end Run;
end Tcp_Server;
