with Logger;

package body Speed_Daemon.Fsm is
   protected body Control is
      function Current (Socket : GNAT.Sockets.Socket_Type)
         return Device_State is
         use type Socket_State.Cursor;
         C : constant Socket_State.Cursor :=
            Known_Sockets.Find (Key => Socket);
      begin
         if C /= Socket_State.No_Element then
            return Socket_State.Element (Position => C);
         else
            return (Device => Unknown, Current_State => Start);
         end if;
      end Current;

      function Map_Message_To_Event (Message : Request)
         return Event is
      begin
         case Message.Tp is
            when Unknown_Message =>
               return Unknown_Message;
            when Plate =>
               return Plate;
            when I_Am_Camera =>
               return I_Am_Camera;
            when I_Am_Dispatcher =>
               return I_Am_Dispatcher;
            when Want_Heartbeat =>
               if Message.Interval = 0 then
                  return Stop_Heartbeat;
               else
                  return Start_Heartbeat;
               end if;
         end case;
      end Map_Message_To_Event;

      procedure Remove (Socket : GNAT.Sockets.Socket_Type) is
      begin
         --if Known_Sockets.Contains (Key => Socket) then
         Known_Sockets.Exclude (Key => Socket);
      end Remove;

      procedure Do_Event (Socket : GNAT.Sockets.Socket_Type;
         Message : Request;
         Status : out Boolean) is
         E : constant Event :=
            Map_Message_To_Event (Message => Message);
         D : constant Device_State :=
            Current (Socket => Socket);
         T : constant Table_Entry :=
            Transitions (D.Device, D.Current_State, E);
      begin
         Logger.Log ("Start state : Device: " &
            D.Device'Image &
            "State: " &
            D.Current_State'Image);
         Logger.Log ("Next State: Device: " &
                     T.Next_State.Device'Image &
                     " State: " &
                     T.Next_State.Current_State'Image);
         if T.Effect /= null then
            Status := T.Effect.all (Socket => Socket,
               Message => Message);
         end if;
         if T.Next_State.Current_State = Invalid then
            Known_Sockets.Exclude (Key => Socket);
            Status := False;
         else
            Known_Sockets.Include (Key => Socket,
               New_Item => T.Next_State);
            Status := True;
            Logger.Log ("Inserted new state");
         end if;
      end Do_Event;
   end Control;

   procedure Initialize (Transitions_In : Transition_Map) is
   begin
      Transitions := Transitions_In;
   end Initialize;
end Speed_Daemon.Fsm;
