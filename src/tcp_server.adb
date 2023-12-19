with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package body Tcp_Server is
   package TIO renames Ada.Text_IO;
   package Sockets renames GNAT.Sockets;
   package Exceptions renames Ada.Exceptions;
   package TID renames Ada.Task_Identification;

   task type Connection_Handler is
      entry Start (Client_Socket : Sockets.Socket_Type);
   end Connection_Handler;
   task body Connection_Handler is
      Socket : Sockets.Socket_Type;
   begin
      accept Start (Client_Socket : Sockets.Socket_Type) do
         Socket := Client_Socket;
      end Start;
      TIO.Put_Line ("Task_ID: " & TID.Image (TID.Current_Task));
      Client (Socket => Socket);
      Sockets.Close_Socket (Socket);
   exception
      when Except : others =>
         TIO.Put_Line ("Task died: " 
                       & Exceptions.Exception_Name (Except)
                       & " "
                       & Exceptions.Exception_Message (Except));
   end Connection_Handler;

   type Connection_Handler_Ptr is access Connection_Handler;
   procedure Free_Connection_Handler_Ptr is
      new Ada.Unchecked_Deallocation (Object => Connection_Handler,
                                      Name => Connection_Handler_Ptr);

   package Tasks_Vector is 
      new Ada.Containers.Vectors (Index_Type => Natural,
                                  Element_Type => Connection_Handler_Ptr);
   Running_Tasks : Tasks_Vector.Vector;

   procedure Purge_Terminated_Tasks is
      Terminated, Still_Running : Tasks_Vector.Vector;
   begin
      for E of Running_Tasks loop
         if E'Terminated then
            Terminated.Append (New_Item => E);
         else
            Still_Running.Append (New_Item => E);
         end if;
      end loop;
      Running_Tasks := Still_Running;
      for E of Terminated loop
         TIO.Put_Line ("freeing a task");
         Free_Connection_Handler_Ptr (X => E);
      end loop;
   end Purge_Terminated_Tasks;

   procedure Run (Callback : Data_Handler) is
      Address  : Sockets.Sock_Addr_Type;
      Server   : Sockets.Socket_Type;
      Host_Name : constant String := Sockets.Host_Name;
      Host_Entry : constant Sockets.Host_Entry_Type :=
         Sockets.Get_Host_By_Name (Host_Name);
   begin
      Client := Callback;
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
            Handler_Task : Connection_Handler_Ptr :=
               new Connection_Handler;
         begin
            Sockets.Accept_Socket (Server, Socket, Address);
            Handler_Task.Start (Client_Socket => Socket);
            Running_Tasks.Append (New_Item => Handler_Task);
         end;
         Purge_Terminated_Tasks;
      end loop;
      Sockets.Close_Socket (Server);
   exception when E : others =>
      TIO.Put_Line (Exceptions.Exception_Name (E) &
                    ": " &
                    Exceptions.Exception_Message (E));
   end Run;
end Tcp_Server;
