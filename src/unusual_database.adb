with Ada.Streams;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with GNAT.Sockets;

package body Unusual_Database is
   package Key_Value_Store is
      new Ada.Containers.Indefinite_Hashed_Maps (Key_Type => String,
                                                 Element_Type => String,
                                                 Hash => Ada.Strings.Hash,
                                                 Equivalent_Keys => "=");
   Database : Key_Value_Store.Map;

   type Message_Type is (Insert, Retrieve, Version);
   function Get_Message_Type (S : String)
      return Message_Type is
   begin
      if S = "version" then
         return Version;
      elsif Ada.Strings.Fixed.Index (Source => S,
                                     Pattern => "=") /= 0
      then
         return Insert;
      else
         return Retrieve;
      end if;
   end Get_Message_Type;

   function To_String (Buffer : Ada.Streams.Stream_Element_Array)
      return String is
      Result : String (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Result (Integer (I)) := Character'Val (Buffer (I));
      end loop;
      return Result;
   end To_String;

   function To_Stream_Element_Array (S : String)
      return Ada.Streams.Stream_Element_Array is
      Result : Ada.Streams.Stream_Element_Array (1 .. S'Length);
   begin
      for I in S'Range loop
         Result (Ada.Streams.Stream_Element_Offset (I - S'First + 1)) :=
            Character'Pos (S (I));
      end loop;
      return Result;
   end To_Stream_Element_Array;

   function Key (S : String)
      return String is
      Idx : constant Natural :=
         Ada.Strings.Fixed.Index (Source => S,
                                  Pattern => "=");
   begin
      return S (S'First .. Idx - 1);
   end Key;

   function Value (S : String)
      return String is
      Idx : constant Natural :=
         Ada.Strings.Fixed.Index (Source => S,
                                  Pattern => "=");
   begin
      return S (Idx + 1 .. S'Last);
   end Value;

   procedure Run is
      Server : GNAT.Sockets.Socket_Type;
      Address, From : GNAT.Sockets.Sock_Addr_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last : Ada.Streams.Stream_Element_Offset;
      Watchdog : Natural := 0;
      Version_Data : constant Ada.Streams.Stream_Element_Array :=
         To_Stream_Element_Array (S => "version=Ken's Key-Value Store 1.0");

   begin
      GNAT.Sockets.Create_Socket (Server,
                                  GNAT.Sockets.Family_Inet,
                                  GNAT.Sockets.Socket_Datagram);
      GNAT.Sockets.Set_Socket_Option (Server,
                                      GNAT.Sockets.Socket_Level,
                                      (GNAT.Sockets.Reuse_Address, True));
      Address.Addr := GNAT.Sockets.Any_Inet_Addr;

      Address.Port := 5432;
      GNAT.Sockets.Bind_Socket (Server, Address);
      loop
         begin
            GNAT.Sockets.Receive_Socket (Server, Data, Last, From);
            declare
               Message : constant String :=
                  To_String (Buffer => Data (Data'First .. Last));
               Tp : constant Message_Type :=
                  Get_Message_Type (S => Message);
            begin
               case Tp is
                  when Version =>
                     GNAT.Sockets.Send_Socket (Socket => Server,
                                               Item => Version_Data,
                                               Last => Last,
                                               To => From);
                  when Insert =>
                     declare
                        K : constant String :=
                           Key (S => Message);
                        V : constant String :=
                           Value (S => Message);
                     begin
                        if K /= "version" then
                           Database.Include (K, V);
                        end if;
                     end;
                  when Retrieve =>
                     if Database.Contains (Message) then
                        declare
                           Response : constant Ada.Streams.Stream_Element_Array :=
                              To_Stream_Element_Array (S => Message &
                                                            "=" &
                                                            Database (Message));
                        begin
                           GNAT.Sockets.Send_Socket (Socket => Server,
                                                     Item => Response,
                                                     Last => Last,
                                                     To => From);
                        end;
                     end if;
               end case;
            end;
         exception
            when Except : GNAT.Sockets.Socket_Error =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (Except) &
                                     " " &
                                     Ada.Exceptions.Exception_Message (Except));
               Watchdog := Watchdog + 1;
               exit when Watchdog = 10;
         end;
      end loop;
   end Run;
end Unusual_Database;
