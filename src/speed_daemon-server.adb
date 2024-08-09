with GNAT.Sockets;
--  with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;

with Logger;
with Select_Server;
with Speed_Daemon.Fsm;
with Speed_Daemon.Network;
with Speed_Daemon.Heartbeat_Manager;

package body Speed_Daemon.Server is
   package Socket_Camera_Store is
      new Ada.Containers.Indefinite_Hashed_Maps (
         Key_Type => GNAT.Sockets.Socket_Type,
         Element_Type => Device,
         Hash => Socket_Hash,
         Equivalent_Keys => "=");

   package Socket_Dispatcher_Store is
      new Ada.Containers.Indefinite_Hashed_Maps (
         Key_Type => GNAT.Sockets.Socket_Type,
         Element_Type => Device,
         Hash => Socket_Hash,
         Equivalent_Keys => "=");

   Socket_Camera_Map : Socket_Camera_Store.Map;
   Socket_Dispatcher_Map : Socket_Dispatcher_Store.Map;

   function Add_Camera (Socket : GNAT.Sockets.Socket_Type;
      Message : Request)
      return Boolean is
   begin
      Socket_Camera_Map.Include (Key => Socket,
         New_Item => (Tp => Camera,
                      Road => Message.Road, 
                      Mile => Message.Mile, 
                      Speed_Limit => Message.Speed_Limit));
      return True;
   end Add_Camera;

   procedure Cleanup_Socket (
      Socket : GNAT.Sockets.Socket_Type) is
   begin
      Speed_Daemon.Fsm.Control.Remove (Socket => Socket);
      Speed_Daemon.Heartbeat_Manager.Stop_Sending (Socket => Socket);
      Socket_Camera_Map.Exclude (Key => Socket);
      Socket_Dispatcher_Map.Exclude (Key => Socket);
   end Cleanup_Socket;

   function Send_Error_Response (Socket : GNAT.Sockets.Socket_Type;
      Message : Request)
      return Boolean is
   begin
      Logger.Log ("sending error response");
      Speed_Daemon.Network.Send_Error_Response (Socket => Socket);
      return False;
   end Send_Error_Response;

   function Start_Heartbeat (Socket : GNAT.Sockets.Socket_Type;
      Message : Request)
      return Boolean is
   begin
      Speed_Daemon.Heartbeat_Manager.Start_Sending (Socket => Socket,
         Period_In_Milliseconds => Message.Interval * 100);
      return True;
   end Start_Heartbeat;

   function Message_Handler (Socket : GNAT.Sockets.Socket_Type)
      return Select_Server.Socket_Status is
      Socket_Closed : Boolean;
      Message : Speed_Daemon.Request :=
         Speed_Daemon.Network.Receive (Socket => Socket,
            Socket_Closed => Socket_Closed);
   begin
      Logger.Log ("SOCKET: " & GNAT.Sockets.Image (Socket => Socket));
      Logger.Log ("Message.Tp " & Message.Tp'Image);
      Logger.Log ("Socket closed " & Socket_Closed'Image);

      if Socket_Closed then
         Cleanup_Socket (Socket => Socket);
         return Select_Server.Close;
      end if;

      declare
         Fsm_Status : Boolean;
      begin
         Speed_Daemon.Fsm.Control.Do_Event (Socket => Socket,
            Message => Message,
            Status => Fsm_Status);
         Logger.Log ("Fsm status: " & Fsm_Status'Image);
         if Fsm_Status = False then
            Cleanup_Socket (Socket => Socket);
            return Select_Server.Close;
         else
            return Select_Server.Healthy;
         end if;
      end;

      --  Socket_Hashe Message.Tp is
      --   when Want_Heartbeat =>
      --      Logger.Log ("got heartbeat message");
      --      if not Is_Known (Socket => Socket) then
      --         Logger.Log ("got heartbeat message from an unknown source");
      --         Speed_Daemon.Network.Send_Error_Response (
      --            Socket => Socket);
      --         return Select_Server.Close;
      --      else
      --         if Message.Interval /= 0 then
      --            Speed_Daemon.Heartbeat_Manager.Start_Sending (
     --                Socket => Socket,
     --                Period_In_Milliseconds => Message.Interval * 100);
     --          else
     --             Speed_Daemon.Heartbeat_Manager.Stop_Sending (
     --                Socket => Socket);
     --          end if;
     --       end if;
     --    when Unknown_Message =>
     --       Logger.Log ("got unknown message");
     --       Speed_Daemon.Network.Send_Error_Response (
     --          Socket => Socket);
     --       return Select_Server.Close;
     --    when others =>
     --       return Select_Server.Close;
     -- end case;

     --  return Select_Server.Healthy;
   end Message_Handler;

   procedure Run is
   begin
      Select_Server.Run (Callback => Message_Handler'Access);
   end Run;

   use Speed_Daemon.Fsm;
   Transitions : constant Speed_Daemon.Fsm.Transition_Map :=
      (Camera =>
         (Identified =>
            (Unknown_Message => ((Camera, Invalid), Send_Error_Response'Access),
            I_Am_Camera => ((Camera, Invalid), Send_Error_Response'Access),
            I_Am_Dispatcher => ((Camera, Invalid), Send_Error_Response'Access),
            Start_Heartbeat => ((Camera, Heartbeat_Enabled), Start_Heartbeat'Access),
            Stop_Heartbeat => ((Camera, Invalid), Send_Error_Response'Access),
            Plate => ((Camera, Identified), null)),
         Heartbeat_Enabled =>
            (Unknown_Message => ((Camera, Invalid), Send_Error_Response'Access),
            I_Am_Camera => ((Camera, Invalid), Send_Error_Response'Access),
            I_Am_Dispatcher => ((Camera, Invalid), Send_Error_Response'Access),
            Start_Heartbeat => ((Camera, Invalid), Send_Error_Response'Access),
            Stop_Heartbeat => ((Camera, Identified), null),
            Plate => ((Camera, Heartbeat_Enabled), null)),
         others => (others => ((Camera, Invalid), Send_Error_Response'Access))),
      Dispatcher =>
         (Identified =>
            (Unknown_Message => ((Dispatcher, Invalid), Send_Error_Response'Access),
            I_Am_Camera => ((Dispatcher, Invalid), Send_Error_Response'Access),
            I_Am_Dispatcher => ((Dispatcher, Invalid), Send_Error_Response'Access),
            Start_Heartbeat => ((Dispatcher, Heartbeat_Enabled), null),
            Stop_Heartbeat => ((Dispatcher, Invalid), Send_Error_Response'Access),
            Plate => ((Dispatcher, Invalid), Send_Error_Response'Access)),
         Heartbeat_Enabled =>
            (Unknown_Message => ((Dispatcher, Invalid), Send_Error_Response'Access),
            I_Am_Camera => ((Dispatcher, Invalid), Send_Error_Response'Access),
            I_Am_Dispatcher => ((Dispatcher, Invalid), Send_Error_Response'Access),
            Start_Heartbeat => ((Dispatcher, Invalid), Send_Error_Response'Access),
            Stop_Heartbeat => ((Dispatcher, Identified), null),
            Plate => ((Dispatcher, Invalid), Send_Error_Response'Access)),
         others => (others => ((Dispatcher, Invalid), Send_Error_Response'Access))),
      Unknown =>
         (Start =>
            (Unknown_Message => (Next_State => (Unknown, Invalid), Effect => Send_Error_Response'Access),
            I_Am_Camera => ((Camera, Identified), Add_Camera'Access),
            I_Am_Dispatcher => ((Dispatcher, Identified), null),
            Start_Heartbeat => ((Unknown, Invalid), Send_Error_Response'Access),
            Stop_Heartbeat => ((Unknown, Invalid), Send_Error_Response'Access),
            Plate => ((Unknown, Invalid), Send_Error_Response'Access)),
         others => (others => ((Unknown, Invalid), Send_Error_Response'Access))));
begin
   Speed_Daemon.Fsm.Initialize(Transitions_In => Transitions);
end Speed_Daemon.Server;
