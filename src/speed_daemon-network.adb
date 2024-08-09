with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Logger;

package body Speed_Daemon.Network is
   use type Ada.Streams.Stream_Element_Offset;

   function Hex (Byte : Interfaces.Unsigned_8) return String is
      use type Interfaces.Unsigned_8;
      Hex_Chars : constant array (Interfaces.Unsigned_8 range 0 .. 15)
         of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                          'A', 'B', 'C', 'D', 'E', 'F');
      Half_Byte_1 : constant Interfaces.Unsigned_8 := Byte mod 16;
      Half_Byte_2 : constant Interfaces.Unsigned_8 := Byte / 16;
   begin
      return Hex_Chars (Half_Byte_2) & Hex_Chars (Half_Byte_1);
   end Hex;

   procedure Reverse_Buffer (Data : in out Ada.Streams.Stream_Element_Array) is
      Datum : Ada.Streams.Stream_Element;
   begin
      for I in Data'First .. Data'Last / 2 loop
         Datum := Data (I);
         Data (I)  := Data (Data'Last - I + Data'First);
         Data (Data'Last - I + Data'First) := Datum;
      end loop;
   end Reverse_Buffer;

   generic
      type T is mod <>;
   function Read_Unsigned (
      S : GNAT.Sockets.Socket_Type;
      Item : out T)
      return Boolean;

   function Read_Unsigned (
      S : GNAT.Sockets.Socket_Type;
      Item : out T)
      return Boolean is
      Data : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (T'Size / 8));
      Last : Ada.Streams.Stream_Element_Offset;
      function To_T is
         new Ada.Unchecked_Conversion (Source => Ada.Streams.Stream_Element_Array,
            Target => T);
   begin
      GNAT.Sockets.Receive_Socket (Socket => S,
         Item => Data,
         Last => Last);
      if Last = 0 then
         Logger.Log ("Read_Unsigned: Last is 0");
         return False;
      end if;

      Reverse_Buffer (Data);
      Item := To_T (Data);
      return True;
   exception
      when Except : others =>
         Logger.Log ("Read_Unsigned: " &
            Ada.Exceptions.Exception_Name (Except) &
            " " &
            Ada.Exceptions.Exception_Message (Except));
         return False;
   end Read_Unsigned;

   function Read_U8 is
      new Read_Unsigned (T => Interfaces.Unsigned_8);

   function Read_U16 is
      new Read_Unsigned (T => Interfaces.Unsigned_16);

   function Read_U32 is
      new Read_Unsigned (T => Interfaces.Unsigned_32);

   function To_Request (
      Socket : GNAT.Sockets.Socket_Type;
      Socket_Closed : out Boolean)
      return Request is
      use type Interfaces.Unsigned_8;
      Unknown : Request (Tp => Unknown_Message);
      Request_Type : Interfaces.Unsigned_8;
   begin
      Socket_Closed := False;
      if not Read_U8 (S => Socket, 
            Item => Request_Type)
      then
         Socket_Closed := True;
         return Unknown;
      end if;

      case Request_Type is
         when 16#40# =>
            declare
               R : Request (Tp => Want_Heartbeat);
               U32 : Interfaces.Unsigned_32;
            begin
               if Read_U32 (Socket, U32) then
                  Logger.Log (U32'Image);
                  R.Interval := Positive (U32);
                  return R;
               else
                  Logger.Log ("error reading u32");
                  Socket_Closed := True;
                  return Unknown;
               end if;
            end;
         when 16#80# =>
            declare
               R : Request (Tp => I_Am_Camera);
               Road, Mile, Speed  : Interfaces.Unsigned_16;
            begin
               if Read_U16 (Socket, Road)
                  and then Read_U16 (Socket, Mile)
                  and then Read_U16 (Socket, Speed)
               then
                  R.Road := Road_Number (Road);
                  R.Mile := Positive (Mile);
                  R.Speed_Limit := Positive (Speed);
                  return R;
               else
                  Logger.Log ("Error reading I am camera");
                  Socket_Closed := True;
                  return Unknown;
               end if;
            end;
         when others =>
            Logger.Log (Request_Type'Image);
            Logger.Log ("returning unknown");
            return Unknown;
      end case;
   end To_Request;

   function Receive (Socket : GNAT.Sockets.Socket_Type;
      Socket_Closed : out Boolean)
      return Request is
      (To_Request (Socket => Socket,
         Socket_Closed => Socket_Closed));

   procedure Send_Error_Response (
      Socket : GNAT.Sockets.Socket_Type;
      Message : String := "illegal msg") is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Message'Length + 2);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Buffer (1) := 16#10#;
      Buffer (2) := Message'Length;
      for I in Message'Range loop
         Buffer (Ada.Streams.Stream_Element_Offset (2 + I)) :=
            Character'Pos (Message (I));
      end loop;
      GNAT.Sockets.Send_Socket (Socket => Socket,
         Item => Buffer,
         Last => Last);
   end Send_Error_Response;

   procedure Send_Heartbeat (
      Socket : GNAT.Sockets.Socket_Type) is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Buffer (1) := 16#41#;
      GNAT.Sockets.Send_Socket (Socket => Socket,
         Item => Buffer,
         Last => Last);
   end Send_Heartbeat;
end Speed_Daemon.Network;
