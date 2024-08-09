with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

with Logger;
with Speed_Daemon.Network;

package body Speed_Daemon.Heartbeat_Manager is
   task type Heartbeat is
      entry Start (Period_In_Milliseconds : Positive);
   end Heartbeat;

   type Heartbeat_Ptr is access Heartbeat;
   procedure Free_Heartbeat_Ptr is
      new Ada.Unchecked_Deallocation (Object => Heartbeat,
         Name => Heartbeat_Ptr);

   type Task_State is (Running, Termination_Requested);

   package Socket_Sets is
      new Ada.Containers.Hashed_Sets (Element_Type => GNAT.Sockets.Socket_Type,
         Hash => Socket_Hash,
         Equivalent_Elements => "=");

   type Heartbeat_Task_State is
      record
         H : Heartbeat_Ptr;
         S : Task_State;
         Sockets : Socket_Sets.Set := Socket_Sets.Empty_Set;
      end record;

   function Positive_Hash (P : Positive)
      return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (P));

   package Heartbeat_Task_Store is
      new Ada.Containers.Hashed_Maps (Key_Type => Positive,
         Element_Type => Heartbeat_Task_State,
         Hash => Positive_Hash,
         Equivalent_Keys => "=");

   package Socket_To_Positive is
      new Ada.Containers.Hashed_Maps (Key_Type => GNAT.Sockets.Socket_Type,
         Element_Type => Positive,
         Hash => Socket_Hash,
         Equivalent_Keys => "=");

   protected Manager is
      procedure Add_Socket (Socket : GNAT.Sockets.Socket_Type;
         Period_In_Milliseconds : Positive);
      procedure Active_Sockets (Period_In_Milliseconds : Positive;
         Sockets : out Socket_Sets.Set);
      procedure Remove_Socket (Socket : GNAT.Sockets.Socket_Type);
      --  procedure Remove_Task (Socket : GNAT.Sockets.Socket_Type);
      --  function Is_Running (Socket : GNAT.Sockets.Socket_Type)
      --   return Boolean;
      --  function Is_Terminated (Socket : GNAT.Sockets.Socket_Type)
      --   return Boolean;
      --  procedure Purge_Terminated_Tasks;
   private
      Socket_To_Period : Socket_To_Positive.Map;
      Active_Tasks : Heartbeat_Task_Store.Map;
   end Manager;

   protected body Manager is
      procedure Add_Socket (Socket : GNAT.Sockets.Socket_Type;
         Period_In_Milliseconds : Positive) is
      begin
         if Active_Tasks.Contains (Key => Period_In_Milliseconds) then
            Logger.Log ("Task for " & Period_In_Milliseconds'Image & " exists.");
            Active_Tasks (Period_In_Milliseconds).Sockets.Include (
               New_Item => Socket);
            Socket_To_Period.Include (Key => Socket,
               New_Item => Period_In_Milliseconds);
         else
            Logger.Log ("Task for " & Period_In_Milliseconds'Image & " does not exist.");
            declare
               T : constant Heartbeat_Ptr := new Heartbeat;
               New_State : Heartbeat_Task_State;
            begin
               New_State.H := T;
               New_State.S := Running;
               New_State.Sockets.Include (New_Item => Socket);
               Active_Tasks.Include (Key => Period_In_Milliseconds,
                  New_Item => New_State);
               Socket_To_Period.Include (Key => Socket,
                  New_Item => Period_In_Milliseconds);
               T.Start (Period_In_Milliseconds => Period_In_Milliseconds);
            end;
         end if;
      end Add_Socket;

      procedure Remove_Socket (Socket : GNAT.Sockets.Socket_Type) is
         Period : Positive;
      begin
         if Socket_To_Period.Contains (Key => Socket) then
            Period := Socket_To_Period (Key => Socket);
            Logger.Log ("Remove_Socket: " &
               GNAT.Sockets.Image (Socket) &
               " Period: " &
               Period'Image);
            Active_Tasks (Period).Sockets.Delete (Socket);
            if Active_Tasks (Period).Sockets.Is_Empty then
               Logger.Log ("Removing active task: " &
                  GNAT.Sockets.Image (Socket));
               Active_Tasks.Exclude (Key => Period);
            end if;
            Socket_To_Period.Delete (Key => Socket);
         end if;
      end Remove_Socket;

      procedure Active_Sockets (Period_In_Milliseconds : Positive;
         Sockets : out Socket_Sets.Set) is
      begin
         if Active_Tasks.Contains (Key => Period_In_Milliseconds) then
            Sockets := Active_Tasks (Period_In_Milliseconds).Sockets;
         else 
            Sockets := Socket_Sets.Empty_Set;
         end if;
      end Active_Sockets;

      --  function Is_Running (Socket : GNAT.Sockets.Socket_Type)
      --   return Boolean is
      --   Task_Info : constant Heartbeat_Task_State :=
      --      Active_Tasks.Element (Key => Socket);
      --  begin
      --   return Task_Info.S = Running;
      --  end Is_Running;

      --  function Is_Terminated (Socket : GNAT.Sockets.Socket_Type)
      --   return Boolean is
      --   Task_Info : constant Heartbeat_Task_State :=
      --      Active_Tasks.Element (Key => Socket);
      --  begin
      --   return Task_Info.S = Termination_Requested;
      --  end Is_Terminated;

      --  procedure Purge_Terminated_Tasks is
      --   Terminated_Tasks : Heartbeat_Task_Store.Map;
      --   S : GNAT.Sockets.Socket_Type;
      --   Elt : Heartbeat_Task_State;
      --  begin
      --   for E in Active_Tasks.Iterate loop
      --      S := Heartbeat_Task_Store.Key (E);
      --      Elt := Heartbeat_Task_Store.Element (E);
      --      if Elt.H'Terminated then
      --         Terminated_Tasks.Include (Key => S, New_Item => Elt);
      --      end if;
      --   end loop;

      --   for E in Terminated_Tasks.Iterate loop
      --      S := Heartbeat_Task_Store.Key (E);
      --      Elt := Heartbeat_Task_Store.Element (E);
      --      Free_Heartbeat_Ptr (X => Elt.H);
      --      Active_Tasks.Exclude (Key => S);
      --   end loop;
      --  end Purge_Terminated_Tasks;
   end Manager;

   task body Heartbeat is
      Next_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Period : Ada.Real_Time.Time_Span;
      use type Ada.Real_Time.Time_Span;
      Active : Socket_Sets.Set;
   begin
      accept Start (Period_In_Milliseconds : Positive) do
         Logger.Log ("In Heartbeat start");
         Period := Ada.Real_Time.Milliseconds (Period_In_Milliseconds);
         Next_Time := Ada.Real_Time.Clock;
      end Start;
      begin
         loop
            Logger.Log ("In Heartbeat loop, Period" & Integer (Ada.Real_Time.To_Duration (Period) * 1000)'Image);
            Manager.Active_Sockets (Period_In_Milliseconds => Integer (Ada.Real_Time.To_Duration (Period) * 1000),
               Sockets => Active);
            --  TODO: should we die if Active is empty ?
            exit when Active.Is_Empty;
            for E of Active loop
               begin
                  Logger.Log ("Sending heartbeat to " & GNAT.Sockets.Image (E));
                  Speed_Daemon.Network.Send_Heartbeat (E);
               exception
                  when Except : GNAT.Sockets.Socket_Error =>
                     --  TODO: I feel this task should not remove socket
                     --  by calling Manager.Remove_Socket
                     Logger.Log ("Start: Socket " & 
                        GNAT.Sockets.Image (E) &
                        Ada.Exceptions.Exception_Message (Except));
               end;
            end loop;
            Next_Time := Next_Time + Period;
            delay until Next_Time;
         end loop;
         Logger.Log ("exited heartbeat loop for period: " & 
            Integer (Ada.Real_Time.To_Duration (Period) * 1000)'Image);
      exception
         when Except : others =>
            Logger.Log ("Heartbeat died with exception " &
               Ada.Exceptions.Exception_Name (Except) &
               " " &
               Ada.Exceptions.Exception_Message (Except));
      end;
   end Heartbeat;

   procedure Start_Sending (Socket : GNAT.Sockets.Socket_Type;
      Period_In_Milliseconds : Natural) is
   begin
      Manager.Add_Socket (Socket => Socket,
         Period_In_Milliseconds => Period_In_Milliseconds);
   end Start_Sending;

   procedure Stop_Sending (Socket : GNAT.Sockets.Socket_Type) is
   begin
      Logger.Log ("Removing socket from Heartbeat: " &
         GNAT.Sockets.Image (Socket));
      Manager.Remove_Socket (Socket => Socket);
      Logger.Log ("Removed socket from Heartbeat: " &
         GNAT.Sockets.Image (Socket));
      --  Manager.Purge_Terminated_Tasks;
   end Stop_Sending;

end Speed_Daemon.Heartbeat_Manager;
