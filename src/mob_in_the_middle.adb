with Ada.Streams;
with GNAT.Sockets;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with GNAT.Regpat;

with Logger;
with Task_Per_Connection_Server;

package body Mob_In_The_Middle is
   use type Ada.Streams.Stream_Element_Offset;

   type Socket_Status is (Ready, Timeout, Closed);

   function Has_Data (Socket : GNAT.Sockets.Socket_Type;
      Max_Wait_Time : Duration)
   return Socket_Status is
      Read_Sockets, Write_Sockets : GNAT.Sockets.Socket_Set_Type;
      Selector : GNAT.Sockets.Selector_Type;
      Status : GNAT.Sockets.Selector_Status;
      use type GNAT.Sockets.Selector_Status;
   begin
      GNAT.Sockets.Set (Read_Sockets, Socket);
      GNAT.Sockets.Empty (Write_Sockets);
      GNAT.Sockets.Create_Selector (Selector);
      GNAT.Sockets.Check_Selector (Selector => Selector,
                                   R_Socket_Set => Read_Sockets,
                                   W_Socket_Set => Write_Sockets,
                                   Status => Status,
                                   Timeout => Max_Wait_Time);
      GNAT.Sockets.Empty (Read_Sockets);
      if Status = GNAT.Sockets.Expired then
         return Timeout;
      elsif Status = GNAT.Sockets.Completed then
         return Ready;
      else
         Logger.Log (Item => "Check_Selector returned error "
            & Status'Image);
         return Closed;
      end if;
   end Has_Data;

   function Send (Socket : GNAT.Sockets.Socket_Type;
      Message : String)
   return Boolean is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Message'Length);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      for I in Message'Range loop
         Buffer (Ada.Streams.Stream_Element_Offset (I)) :=
            Character'Pos (Message (I));
      end loop;
      GNAT.Sockets.Send_Socket (Socket => Socket,
                                Item => Buffer,
                                Last => Last);
      return Last /= Buffer'First;
   end Send;

   function Read_Until_Newline (Socket : GNAT.Sockets.Socket_Type;
      Socket_Closed : out Boolean)
   return String is
      Till_Now : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;
      Data : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      loop
         GNAT.Sockets.Receive_Socket (Socket => Socket,
                                      Item => Data,
                                      Last => Last);
         if Last = 0 then
            Socket_Closed := True;
            exit;
         elsif Character'Val (Data (1)) = Ada.Characters.Latin_1.LF then
            Ada.Strings.Unbounded.Append (Source => Till_Now,
                                          New_Item => Character'Val (Data (1)));
            Socket_Closed := False;
            exit;
         else
            Ada.Strings.Unbounded.Append (Source => Till_Now,
                                          New_Item => Character'Val (Data (1)));
         end if;
      end loop;
      return Ada.Strings.Unbounded.To_String (Till_Now);
   end Read_Until_Newline;

   function Tcp_Client (Url : String; Port : Positive)
   return GNAT.Sockets.Socket_Type is
      Client_Host : constant GNAT.Sockets.Host_Entry_Type :=
         GNAT.Sockets.Get_Host_By_Name (Url);
      Client_Inet_Addr : constant String :=
         GNAT.Sockets.Image (Value => GNAT.Sockets.Addresses (E => Client_Host));
      Client_Address : constant GNAT.Sockets.Sock_Addr_Type :=
         (Addr => GNAT.Sockets.Inet_Addr (Client_Inet_Addr),
          Port => GNAT.Sockets.Port_Type (Port),
          Family => GNAT.Sockets.Family_Inet);
      Client : GNAT.Sockets.Socket_Type;
   begin
      GNAT.Sockets.Create_Socket (Client);
      GNAT.Sockets.Connect_Socket (Client, Client_Address);
      return Client;
   end Tcp_Client;

   procedure Search_For_Pattern (Compiled_Expression : GNAT.Regpat.Pattern_Matcher;
      Search_In : String;
      First, Last : out Positive;
      Found : out Boolean) is
      Result : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      GNAT.Regpat.Match (Compiled_Expression, Search_In, Result);
      Found := not GNAT.Regpat."="(Result (1), GNAT.Regpat.No_Match);
      if Found then
         First := Result (1).First;
         Last := Result (1).Last;
      end if;
   end Search_For_Pattern;

   function Replace_All (S : String;
      Find_Pattern : String;
      Replace_With : String;
      From : Positive) return String is
      First, Last : Positive;
      Found : Boolean;
   begin
      Search_For_Pattern (GNAT.Regpat.Compile (Find_Pattern),
         S (From .. S'Last),
         First,
         Last,
         Found);
      if not Found then
         return S;
      end if;

      return Replace_All (S => S (S'First .. First - 1) &
         Replace_With &
         S (Last + 1 .. S'Last),
         Find_Pattern => Find_Pattern,
         Replace_With => Replace_With,
         From => First + Replace_With'Last);
   end Replace_All;

   function Rewrite_Boguscoin_Address (S : String)
      return String is
      Boguscoin_Address_Pattern : constant String :=
         "(?:\s|^)(7[[:alnum:]]{25,34})(?:\s|$)";
   begin
      return Replace_All (S => S,
         Find_Pattern => Boguscoin_Address_Pattern,
         Replace_With => "7YWHMfk9JZe0LM0g1ZauHuiSxhI",
         From => S'First);
   end Rewrite_Boguscoin_Address;

   function Handle_Message (Read_From, Send_To : GNAT.Sockets.Socket_Type)
      return Boolean is
      Socket_Closed : Boolean;
      Message : constant String :=
         Read_Until_Newline (Read_From, Socket_Closed);
      New_Message : constant String :=
         Rewrite_Boguscoin_Address (S => Message);
   begin
      if Socket_Closed then
         return False;
      end if;

      if New_Message'Length = 0 then
         return True;
      end if;

      if Send (Send_To, New_Message) then
         return True;
      else
         return False;
      end if;
   end Handle_Message;

   task type Socket_Bridge is
      entry Start (From_Socket, To_Socket : GNAT.Sockets.Socket_Type);
   end Socket_Bridge;

   task body Socket_Bridge is
      From, To : GNAT.Sockets.Socket_Type;
   begin
      accept Start (From_Socket, To_Socket : GNAT.Sockets.Socket_Type) do
         From := From_Socket;
         To := To_Socket;
      end Start;
      loop
         case Has_Data (Socket => From, Max_Wait_Time => 5.0) is
            when Ready =>
               exit when not Handle_Message (Read_From => From,
                  Send_To => To);
            when Timeout =>
               null;
            when Closed =>
               exit;
         end case;
      end loop;
      begin
         GNAT.Sockets.Close_Socket (Socket => From);
      exception
         when Error : others =>
            Logger.Log (Item => Ada.Exceptions.Exception_Information (Error));
      end;
      begin
         GNAT.Sockets.Close_Socket (Socket => To);
      exception
         when Error : others =>
            Logger.Log (Item => Ada.Exceptions.Exception_Information (Error));
      end;
   end Socket_Bridge;

   procedure Handler (Socket : GNAT.Sockets.Socket_Type) is
      Chat_Server_Client : GNAT.Sockets.Socket_Type;
      From_Client_To_Server, From_Server_To_Client : Socket_Bridge;
   begin
      Chat_Server_Client := Tcp_Client (Url => "chat.protohackers.com",
         Port => 16963);
      From_Client_To_Server.Start (From_Socket => Socket,
         To_Socket => Chat_Server_Client);
      From_Server_To_Client.Start (From_Socket => Chat_Server_Client,
         To_Socket => Socket);
   end Handler;

   procedure Run is
   begin
      Task_Per_Connection_Server.Run (Callback => Handler'Access);
   end Run;
end Mob_In_The_Middle;
