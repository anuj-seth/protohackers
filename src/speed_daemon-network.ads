--  with Ada.Strings.Unbounded;
with GNAT.Sockets;
with Interfaces;

package Speed_Daemon.Network is
   --type Request (Tp : Incoming_Message) is
   --   record
   --      case Tp is
   --         when Want_Heartbeat =>
   --            Interval : Positive;
   --         when others =>
   --            null;
   --      end case;
   --   end record;

   --type Response (Tp : Outgoing_Message) is
   --   record
   --      case Tp is
   --         when Error =>
   --            Message : Ada.Strings.Unbounded.Unbounded_String;
   --         when others =>
   --            null;
   --      end case;
   --   end record;

   function Receive (Socket : GNAT.Sockets.Socket_Type;
      Socket_Closed : out Boolean)
      return Request;

   procedure Send_Error_Response (
      Socket : GNAT.Sockets.Socket_Type;
      Message : String := "illegal msg");

   procedure Send_Heartbeat (
      Socket : GNAT.Sockets.Socket_Type);
end Speed_Daemon.Network;
