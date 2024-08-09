with Ada.Text_IO;
with Ada.Exceptions;

with Logger;

package body Select_Server is
   procedure Run (Callback : Data_Handler) is
      Address : GNAT.Sockets.Sock_Addr_Type;
      Server_Socket : GNAT.Sockets.Socket_Type;
      Host_Name : constant String := GNAT.Sockets.Host_Name;
      Host_Entry : constant GNAT.Sockets.Host_Entry_Type :=
         GNAT.Sockets.Get_Host_By_Name (Host_Name);
      Sockets : GNAT.Sockets.Socket_Set_Type;
      Write_Sockets : GNAT.Sockets.Socket_Set_Type;
      Selector : GNAT.Sockets.Selector_Type;
      Next_Socket_Status : Socket_Status;
   begin
      Address.Addr := GNAT.Sockets.Addresses (Host_Entry,
                                              1);
      Address.Port := 5432;
      GNAT.Sockets.Create_Socket (Server_Socket);
      GNAT.Sockets.Set_Socket_Option (Server_Socket,
                                      GNAT.Sockets.Socket_Level,
                                      (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket (Server_Socket, Address);
      GNAT.Sockets.Listen_Socket (Socket => Server_Socket,
                             Length => 5);
      GNAT.Sockets.Set (Sockets, Server_Socket);
      GNAT.Sockets.Create_Selector (Selector);
      loop
         Logger.Log (Item => "at the top of loop");
         declare
            Read_Sockets : GNAT.Sockets.Socket_Set_Type;
            Status : GNAT.Sockets.Selector_Status;
            use type GNAT.Sockets.Selector_Status;
         begin
            GNAT.Sockets.Copy (Sockets, Read_Sockets);
            GNAT.Sockets.Check_Selector (Selector,
                                         Read_Sockets,
                                         Write_Sockets,
                                         Status);
            if Status = GNAT.Sockets.Completed then
               declare
                  Socket : GNAT.Sockets.Socket_Type;
                  use type GNAT.Sockets.Socket_Type;
               begin
                  GNAT.Sockets.Get (Read_Sockets, Socket);
                  if Socket = Server_Socket then
                     Logger.Log (Item => "accepting");
                     GNAT.Sockets.Accept_Socket (Server_Socket,
                                                 Socket,
                                                 Address);
                     GNAT.Sockets.Set (Sockets, Socket);
                  elsif Socket = GNAT.Sockets.No_Socket then
                     Logger.Log (Item => "server got no_socket");
                  else
                     Logger.Log (Item => "got some data or maybe socket closed");
                     Next_Socket_Status := Callback (Socket => Socket);
                     if Next_Socket_Status = Close then
                        Logger.Log ("closing socket");
                        GNAT.Sockets.Close_Socket (Socket);
                        GNAT.Sockets.Clear (Sockets, Socket);
                     end if;
                  end if;
               end;
            else
               Logger.Log ("Check_Selector returned: " &
                           Status'Img);
            end if;
            GNAT.Sockets.Empty (Read_Sockets);
         end;
      end loop;
      GNAT.Sockets.Close_Socket (Server_Socket);
   exception when E : others =>
      Logger.Log (Ada.Exceptions.Exception_Name (E) &
                  ": " &
                  Ada.Exceptions.Exception_Message (E));
   end Run;
end Select_Server;
