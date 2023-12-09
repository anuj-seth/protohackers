with GNAT.Sockets;
with Ada.Streams;

with Tcp_Server;

package body Smoke_Test is
   package Sockets renames GNAT.Sockets;
   package Streams renames Ada.Streams;
   use type Streams.Stream_Element_Offset;

   procedure Echo (Socket : Sockets.Socket_Type) is
      Channel  : Sockets.Stream_Access;
   begin
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
   end Echo;

   procedure Run is
   begin
      Tcp_Server.Run (Callback => Echo'Access);
   end Run;
end Smoke_Test;
