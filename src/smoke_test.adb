with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Streams;

procedure Smoke_Test is
   package Sockets renames GNAT.Sockets;
   package Exceptions renames Ada.Exceptions;
   package TIO renames Ada.Text_IO;
   package Streams renames Ada.Streams;
   use type Streams.Stream_Element_Offset;

   type Socket_Access is access Sockets.Socket_Type;
   procedure Free_Socket_Access is
      new Ada.Unchecked_Deallocation (Object => Sockets.Socket_Type,
                                      Name => Socket_Access);

   task type Reader is
      entry Set_Socket (Client_Socket : Socket_Access);
   end Reader;
   task body Reader is
      Socket : Socket_Access;
      Channel  : Sockets.Stream_Access;
   begin
      accept Set_Socket (Client_Socket : Socket_Access) do
         Socket := Client_Socket;
      end Set_Socket;
      --  Return a stream associated to the connected socket.
      Channel := Sockets.Stream (Socket.all);
      loop
         declare
            Data : Streams.Stream_Element_Array (1 .. 1024);
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Read (Channel.all, Data, Last);
            exit when Last = 0;
            TIO.Put_Line ("-----------------------");
            for I in 1 .. Last loop
               TIO.Put (Character'Val (Data (I)));
            end loop;
            TIO.Put_Line ("-----------------------");
            --  Send same message to server Pong.
            Streams.Write (Channel.all, Data (1 .. Last));
         end;
      end loop;
      Sockets.Close_Socket (Socket.all);
      Free_Socket_Access (X => Socket);
   end Reader;

   Address  : Sockets.Sock_Addr_Type;
   Server   : Sockets.Socket_Type;
   Host_Name : constant String := Sockets.Host_Name;
   Host_Entry : constant Sockets.Host_Entry_Type :=
      Sockets.Get_Host_By_Name (Host_Name);
begin
   --  Get an Internet address of a host (here the local host name).
   --  Note that a host can have several addresses. Here we get
   --  the first one which is supposed to be the official one.
   TIO.Put_Line ("Host Name " & Host_Name);
   Address.Addr := Sockets.Addresses (Host_Entry,
                                         1);
   --  Get a socket address that is an Internet address and a port
   Address.Port := 5432;
   TIO.Put_Line (Sockets.Image (Address.Addr));
   --  The first step is to create a socket. Once created, this
   --  socket must be associated to with an address. Usually only
   --  a server (Pong here) needs to bind an address explicitly.
   --  Most of the time clients can skip this step because the
   --  socket routines will bind an arbitrary address to an unbound
   --  socket.
   Sockets.Create_Socket (Server);
   --  Allow reuse of local addresses.
   Sockets.Set_Socket_Option (Server,
                              Sockets.Socket_Level,
                              (Sockets.Reuse_Address, True));
   Sockets.Bind_Socket (Server, Address);
   --  A server marks a socket as willing to receive connect events.
   TIO.Put_Line ("now listening");
   Sockets.Listen_Socket (Socket => Server,
                          Length => 5);
   --  Once a server calls Listen_Socket, incoming connects events
   --  can be accepted. The returned Socket is a new socket that
   --  represents the server side of the connection. Server remains
   --  available to receive further connections.
   loop
      declare
         Socket : Socket_Access := new Sockets.Socket_Type;
         Line_Reader : Reader;
      begin
         TIO.Put_Line ("now accepting");
         Sockets.Accept_Socket (Server, Socket.all, Address);
         TIO.Put_Line ("accepted");
         Line_Reader.Set_Socket(Client_Socket => Socket);
      end;
   end loop;
   Sockets.Close_Socket (Server);
   exception when E : others =>
      TIO.Put_Line
             (Exceptions.Exception_Name (E) &
              ": " &
              Exceptions.Exception_Message (E));
end Smoke_Test;
