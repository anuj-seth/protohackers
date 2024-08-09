with Ada.Text_IO;
with Ada.Streams;
with Ada.Characters.Handling;
with GNAT.Sockets;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Characters.Latin_1;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Task_Identification;

with Task_Per_Connection_Server;

package body Budget_Chat is
   package TIO renames Ada.Text_IO;
   package TI renames Ada.Task_Identification;
   use type TI.Task_Id;
   package SU renames Ada.Strings.Unbounded;
   package Sockets renames GNAT.Sockets;
   package Streams renames Ada.Streams;
   use type Streams.Stream_Element_Offset;
   package SF renames Ada.Strings.Fixed;
   package STR renames Ada.Strings;

   subtype User_Identifier is String (1 .. 16);
   package User_Identifier_Vector is
      new Ada.Containers.Vectors (Index_Type => Natural,
                                  Element_Type => User_Identifier);

   type User is
      record
         Username : User_Identifier := (others => ' ');
         Socket : Sockets.Socket_Type;
      end record;

   function Task_Identification_Hashed (Id : TI.Task_Id)
   return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (Key => TI.Image (T => Id)));

   package Chat_Users is
      new Ada.Containers.Hashed_Maps (Key_Type => TI.Task_Id,
                                      Element_Type => User,
                                      Hash => Task_Identification_Hashed,
                                      Equivalent_Keys => TI."=");

   type Socket_Status is (Ready, Timeout, Closed);

   function Has_Data (Socket : Sockets.Socket_Type;
      Max_Wait_Time : Duration)
   return Socket_Status is
      Read_Sockets, Write_Sockets : Sockets.Socket_Set_Type;
      Selector : Sockets.Selector_Type;
      Status : Sockets.Selector_Status;
      use type Sockets.Selector_Status;
      Result : Socket_Status;
   begin
      Sockets.Set (Read_Sockets, Socket);
      Sockets.Create_Selector (Selector);
      Sockets.Check_Selector (Selector => Selector,
                              R_Socket_Set => Read_Sockets,
                              W_Socket_Set => Write_Sockets,
                              Status => Status,
                              Timeout => Max_Wait_Time);
      if Status = Sockets.Expired then
         Result := Timeout;
      elsif Status = Sockets.Completed
         and then Sockets.Is_Set (Item => Read_Sockets,
                                  Socket => Socket)
      then
         Result := Ready;
      else
         TIO.Put_Line ("Check_Selector returned error " & Status'Image);
         Result := Closed;
      end if;
      Sockets.Empty (Read_Sockets);
      return Result;
   end Has_Data;

   function Send (Socket : Sockets.Socket_Type;
      Message : String)
   return Boolean is
      Buffer : Streams.Stream_Element_Array (1 .. Message'Length + 1);
      Last : Streams.Stream_Element_Offset;
   begin
      for I in Message'Range loop
         Buffer (Streams.Stream_Element_Offset (I)) :=
            Character'Pos (Message (I));
      end loop;
      Buffer (Buffer'Last) := Character'Pos (Ada.Characters.Latin_1.LF);
      Sockets.Send_Socket (Socket => Socket,
                           Item => Buffer,
                           Last => Last);
      return Last /= Buffer'First;
   end Send;

   function Read_Until_Newline (Socket : Sockets.Socket_Type;
      Socket_Closed : out Boolean)
   return String is
      Till_Now : SU.Unbounded_String := SU.Null_Unbounded_String;
      Data : Streams.Stream_Element_Array (1 .. 1);
      Last : Streams.Stream_Element_Offset;
   begin
      loop
         Sockets.Receive_Socket (Socket => Socket,
                                 Item => Data,
                                 Last => Last);
         if Last = 0 then
            Socket_Closed := True;
            exit;
         elsif Character'Val (Data (1)) = Ada.Characters.Latin_1.LF then
            Socket_Closed := False;
            exit;
         else
            SU.Append (Source => Till_Now,
                       New_Item => Character'Val (Data (1)));
         end if;
      end loop;
      return SU.To_String (Till_Now);
   end Read_Until_Newline;

   protected Logged_In_Users is
      procedure Add_User (Id : TI.Task_Id;
         Username : String;
         Socket : Sockets.Socket_Type);
      procedure Remove_User (Id : TI.Task_Id);
      function My_Username (Id : TI.Task_Id) return User_Identifier;
      function List_Users_Except (Id : TI.Task_Id)
         return User_Identifier_Vector.Vector;
      procedure Send_Message_Except (Id : TI.Task_Id; Message : String);
   private
      Joined_Users : Chat_Users.Map;
   end Logged_In_Users;

   protected body Logged_In_Users is
      procedure Add_User (Id : TI.Task_Id;
         Username : String;
         Socket : Sockets.Socket_Type) is
         Last_Index : Natural;
         User_Id : User_Identifier := (others => ' ');
      begin
         if Username'Last > 16 then
            Last_Index := 16;
         else
            Last_Index := Username'Last;
         end if;
         User_Id (User_Id'First .. Last_Index) :=
            Username (Username'First .. Last_Index);
         Joined_Users.Include (Key => Id,
                               New_Item => (Username => User_Id,
                                            Socket => Socket));
      end Add_User;
      procedure Remove_User (Id : TI.Task_Id) is
      begin
         Joined_Users.Exclude (Key => Id);
      end Remove_User;
      function My_Username (Id : TI.Task_Id) return User_Identifier is
         (Joined_Users (Id).Username);
      function List_Users_Except (Id : TI.Task_Id)
      return User_Identifier_Vector.Vector is
         K : TI.Task_Id;
         V : User;
         Result : User_Identifier_Vector.Vector;
      begin
         for C in Joined_Users.Iterate loop
            K := Chat_Users.Key (Position => C);
            if K /= Id then
               V := Chat_Users.Element (Position => C);
               Result.Append (New_Item => V.Username);
            end if;
         end loop;
         return Result;
      end List_Users_Except;
      procedure Send_Message_Except (Id : TI.Task_Id;
         Message : String) is
         K : TI.Task_Id;
         V : User;
      begin
         for C in Joined_Users.Iterate loop
            K := Chat_Users.Key (Position => C);
            if K /= Id then
               V := Chat_Users.Element (Position => C);
               if not Send (Socket => V.Socket, Message => Message) then
                  TIO.Put_Line ("Sending to socket failed");
               end if;
            end if;
         end loop;
      end Send_Message_Except;
   end Logged_In_Users;

   type Client_State is
      (Connection_Start, Wait_For_Username, Joined,
       Announce_Presence, List_All_Users, Chat, User_Left, Fini);

   function Get_Username (Socket : Sockets.Socket_Type)
   return Client_State is
      function Is_Valid_Username (S : String)
      return Boolean is
         package CH renames Ada.Characters.Handling;
      begin
         if S'Length = 0 then
            return False;
         end if;

         for E of S loop
            if not (CH.Is_Letter (Item => E)
                    or else CH.Is_Digit (Item => E))
            then
               return False;
            end if;
         end loop;
         return True;
      end Is_Valid_Username;

      Socket_State : constant Socket_Status :=
         Has_Data (Socket => Socket,
                   Max_Wait_Time => 5.0);
   begin
      case Socket_State is
         when Ready =>
            declare
               Socket_Closed : Boolean;
               Send_Status : Boolean;
               Username : constant String :=
                  Read_Until_Newline (Socket => Socket,
                                      Socket_Closed => Socket_Closed);
            begin
               if Socket_Closed then
                  return Fini;
               end if;

               if not Is_Valid_Username (S => Username)
               then
                  Send_Status := Send (Socket => Socket,
                                       Message => "Please provide valid user name");
                  return Fini;
               end if;

               Logged_In_Users.Add_User (Id => TI.Current_Task,
                                         Username => Username,
                                         Socket => Socket);
               return Joined;
            end;
         when Timeout =>
            return Wait_For_Username;
         when Closed =>
            return Fini;
      end case;
   end Get_Username;

   function Handle_Chat_Message (Id : TI.Task_Id;
      Username : String;
      Socket : Sockets.Socket_Type)
   return Client_State is
      Socket_State : constant Socket_Status :=
         Has_Data (Socket => Socket,
                   Max_Wait_Time => 5.0);
   begin
      case Socket_State is
         when Ready =>
            declare
               Socket_Closed : Boolean;
               Message : constant String :=
                  Read_Until_Newline (Socket => Socket,
                                      Socket_Closed => Socket_Closed);
            begin
               if Socket_Closed then
                  return User_Left;
               end if;

               Logged_In_Users.Send_Message_Except (Id => Id,
                                                    Message => "["
                                                      & Username
                                                      & "] "
                                                      & Message);
               return Chat;
            end;
         when Timeout =>
            return Chat;
         when Closed =>
            return User_Left;
      end case;
   end Handle_Chat_Message;

   procedure Handler (Socket : Sockets.Socket_Type) is
      Current_State : Client_State := Connection_Start;
      My_Task_Id : constant TI.Task_Id := TI.Current_Task;
      My_Username : User_Identifier;
   begin
      loop
         case Current_State is
            when Connection_Start =>
               if Send (Socket => Socket,
                        Message => "Welcome to budgetchat! "
                           & " What shall I call you?")
               then
                  Current_State := Wait_For_Username;
               else
                  Current_State := Fini;
               end if;
            when Wait_For_Username =>
               Current_State := Get_Username (Socket => Socket);
            when Joined =>
               declare
                  Message : SU.Unbounded_String;
                  Usernames : constant User_Identifier_Vector.Vector :=
                     Logged_In_Users.List_Users_Except (Id => My_Task_Id);
               begin
                  SU.Append (Source => Message,
                             New_Item => "* The room contains: ");
                  for I in Usernames.First_Index .. Usernames.Last_Index loop
                     SU.Append (Source => Message,
                                New_Item => SF.Trim (Source => Usernames (I),
                                                     Side => STR.Right));
                     if I /= Usernames.Last_Index then
                        SU.Append (Source => Message,
                                   New_Item => ", ");
                     end if;
                  end loop;
                  if not Send (Socket => Socket,
                               Message => SU.To_String (Source => Message))
                  then
                     TIO.Put_Line ("sending join message failed");
                     Current_State := Fini;
                  else
                     My_Username :=
                        Logged_In_Users.My_Username (Id => My_Task_Id);
                     Current_State := Announce_Presence;
                  end if;
               end;
            when Announce_Presence =>
               declare
                  User : constant String :=
                     SF.Trim (Source => My_Username, Side => STR.Right);
                  Message : constant String :=
                     "* " & User & " has entered the room";
               begin
                  Logged_In_Users.Send_Message_Except (Id => My_Task_Id,
                                                       Message => Message);
               end;
               Current_State := Chat;
            when Chat =>
               Current_State := Handle_Chat_Message (Id => My_Task_Id,
                                                     Username => SF.Trim (Source => My_Username,
                                                                          Side => STR.Right),
                                                     Socket => Socket);
            when User_Left =>
               declare
                  User : constant String :=
                     SF.Trim (Source => My_Username, Side => STR.Right);
                  Message : constant String :=
                     "* " & User & " has left the room";
               begin
                  Logged_In_Users.Send_Message_Except (Id => My_Task_Id,
                                                       Message => Message);
               end;
               Current_State := Fini;
            when Fini =>
               Logged_In_Users.Remove_User (Id => My_Task_Id);
               exit;
            when others =>
               TIO.Put_Line ("not implemented yet" & Current_State'Image);
               delay 5.0;
         end case;
      end loop;
      Sockets.Close_Socket (Socket);
   end Handler;

   procedure Run is
   begin
      Task_Per_Connection_Server.Run (Callback => Handler'Access);
   end Run;
end Budget_Chat;
