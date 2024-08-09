with Ada.Containers.Hashed_Maps;

package Speed_Daemon.Fsm is
   type State is (
      Start, Identified,
      Heartbeat_Enabled, Invalid);

   type Event is (
      I_Am_Camera, I_Am_Dispatcher,
      Start_Heartbeat, Stop_Heartbeat,
      Plate, Unknown_Message);

   type Device_State is
      record
         Device : Device_Type := Unknown;
         Current_State : State := Start;
      end record;

   type Effect_Ptr is
      access function (Socket : GNAT.Sockets.Socket_Type;
         Message : Request) return Boolean;

   type Table_Entry is
      record
         Next_State : Device_State;
         Effect : Effect_Ptr;
      end record;

   type Transition_Map is array (Device_Type, State, Event)
      of Table_Entry;

   package Socket_State is
      new Ada.Containers.Hashed_Maps (Key_Type => GNAT.Sockets.Socket_Type,
         Element_Type => Device_State,
         Hash => Socket_Hash,
         Equivalent_Keys => "=");

   protected Control is
      procedure Do_Event (Socket : GNAT.Sockets.Socket_Type;
         Message : Request;
         Status : out Boolean);

      function Current (Socket : GNAT.Sockets.Socket_Type)
         return Device_State;

      procedure Remove (Socket : GNAT.Sockets.Socket_Type);

   private
      Known_Sockets : Socket_State.Map;
   end Control;


   procedure Initialize (Transitions_In : Transition_Map);
private
   Transitions : Transition_Map;

end Speed_Daemon.Fsm;

