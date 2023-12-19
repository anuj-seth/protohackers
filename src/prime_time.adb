with Ada.Text_IO;
with GNAT.Sockets;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

with Tcp_Server;
with Prime_Time.Message;

package body Prime_Time is
   package TIO renames Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;
   package Sockets renames GNAT.Sockets;
   package Streams renames Ada.Streams;
   use type Streams.Stream_Element_Offset;

   function Read_Until_Newline (Channel : Sockets.Stream_Access)
   return String is
      Till_Now : SU.Unbounded_String := SU.Null_Unbounded_String;
      Data : Streams.Stream_Element_Array (1 .. 1);
      Last : Streams.Stream_Element_Offset;
   begin
      loop
         Streams.Read (Channel.all, Data, Last);
         exit when Character'Val (Data (1)) = Ada.Characters.Latin_1.LF
            or Last = 0;
         SU.Append (Source => Till_Now, New_Item => Character'Val (Data (1)));
      end loop;
      return SU.To_String (Till_Now);
   end Read_Until_Newline;

   procedure Handler (Socket : Sockets.Socket_Type) is
      Channel  : Sockets.Stream_Access;
   begin
      Channel := Sockets.Stream (Socket);
      loop
         declare
            Success : Boolean;
            Request : constant String :=
               Read_Until_Newline (Channel => Channel);
            Response : constant String :=
               Prime_Time.Message.Message_Handler (Json_String => Request,
                                                   Success => Success);
            Response_Buffer : Streams.Stream_Element_Array (1 .. Response'Length + 1);
         begin
            TIO.Put_Line (Item => "1 " & Request);
            TIO.Put_Line (Item => "2 " & Response);
            for I in Response'Range loop
               Response_Buffer (Streams.Stream_Element_Offset (I)) := Character'Pos (Response (I));
            end loop;
            Response_Buffer (Response_Buffer'Last) := Character'Pos (Ada.Characters.Latin_1.LF);
            Streams.Write (Channel.all, Response_Buffer);
            exit when not Success;
         end;
      end loop;
      Sockets.Close_Socket (Socket);
   end Handler;

   procedure Run is
   begin
      Tcp_Server.Run (Callback => Handler'Access);
   end Run;
end Prime_Time;
