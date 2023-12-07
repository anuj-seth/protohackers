with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

procedure Smoke_Test is
   package Sockets renames GNAT.Sockets;
   package Exceptions renames Ada.Exceptions;
   package TIO renames Ada.Text_IO;
   package Streams renames Ada.Streams;
   use type Streams.Stream_Element_Offset;

   task type Reader is
      entry Set_Socket (Client_Socket : Sockets.Socket_Type);
   end Reader;
   task body Reader is
      Socket : Sockets.Socket_Type;
      Channel  : Sockets.Stream_Access;
   begin
      accept Set_Socket (Client_Socket : Sockets.Socket_Type) do
         Socket := Client_Socket;
      end Set_Socket;
      Channel := Sockets.Stream (Socket);
      loop
         declare
            Data : Streams.Stream_Element_Array (1 .. 1024);
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Read (Channel.all, Data, Last);
            exit when Last = 0;
            Streams.Write (Channel.all, Data (1 .. Last));
         end;
      end loop;
      Sockets.Close_Socket (Socket);
   end Reader;

   Address  : Sockets.Sock_Addr_Type;
   Server   : Sockets.Socket_Type;
   Host_Name : constant String := Sockets.Host_Name;
   Host_Entry : constant Sockets.Host_Entry_Type :=
      Sockets.Get_Host_By_Name (Host_Name);
begin
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
         Line_Reader : Reader;
      begin
         Sockets.Accept_Socket (Server, Socket, Address);
         Line_Reader.Set_Socket (Client_Socket => Socket);
      end;
   end loop;
   Sockets.Close_Socket (Server);
   exception when E : others =>
      TIO.Put_Line
             (Exceptions.Exception_Name (E) &
              ": " &
              Exceptions.Exception_Message (E));
end Smoke_Test;
