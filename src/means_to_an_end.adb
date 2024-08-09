with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Containers.Ordered_Sets;
with GNAT.Sockets;
with Interfaces;
with System;

with Task_Per_Connection_Server;

package body Means_To_An_End is
   package TIO renames Ada.Text_IO;
   package Sockets renames GNAT.Sockets;
   package Streams renames Ada.Streams;
   use type Streams.Stream_Element_Offset;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_32;

   SU : constant := System.Storage_Unit;
   subtype SEA is Ada.Streams.Stream_Element_Array;
   generic function UC renames Ada.Unchecked_Conversion;
   subtype S_I is SEA (1 .. (Integer'Size + SU - 1) / SU);
   function From_I is new UC (Integer, S_I);

   function Hex (Byte : Interfaces.Unsigned_8) return String is
      Hex_Chars : constant array (Interfaces.Unsigned_8 range 0 .. 15)
         of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                          'A', 'B', 'C', 'D', 'E', 'F');
      Half_Byte_1 : constant Interfaces.Unsigned_8 := Byte mod 16;
      Half_Byte_2 : constant Interfaces.Unsigned_8 := Byte / 16;
   begin
      return Hex_Chars (Half_Byte_2) & Hex_Chars (Half_Byte_1);
   end Hex;

   procedure Write_Big_Endian (Stream : access Streams.Root_Stream_Type'Class;
      W : SEA) is
      Out_Buf : SEA (1 .. 4);
   begin
      Out_Buf (1) := W (4);
      Out_Buf (2) := W (3);
      Out_Buf (3) := W (2);
      Out_Buf (4) := W (1);

      Streams.Write (Stream.all, Out_Buf);
   end Write_Big_Endian;

   type Price is
      record
         Timestamp : Integer;
         Price : Integer;
      end record;

   function Less_Than (Left, Right : Price)
   return Boolean is
      (Left.Timestamp < Right.Timestamp);

   function Equal (Left, Right : Price)
   return Boolean is
      (Left.Timestamp = Right.Timestamp);

   package Prices is
      new Ada.Containers.Ordered_Sets (Element_Type => Price,
                                       "<" => Less_Than,
                                       "=" => Equal);
   function Mean_Between (Asset_Prices : Prices.Set;
      Min_Time, Max_Time : Integer)
   return Integer is
      Sum_Of_Prices : Long_Integer := 0;
      Count_Of_Assets : Long_Integer := 0;
   begin
      if Min_Time > Max_Time then
         return 0;
      end if;

      for E of Asset_Prices loop
         exit when E.Timestamp > Max_Time;
         if E.Timestamp >= Min_Time then
            Sum_Of_Prices := Sum_Of_Prices + Long_Integer (E.Price);
            Count_Of_Assets := Count_Of_Assets + 1;
         end if;
      end loop;
      if Count_Of_Assets = 0 then
         return 0;
      else
         return Integer (Sum_Of_Prices / Count_Of_Assets);
      end if;
   end Mean_Between;

   function To_Integer (Bytes : Streams.Stream_Element_Array)
   return Integer is
      W : Interfaces.Unsigned_32;
      Idx : constant Streams.Stream_Element_Offset :=
         Bytes'First;
      Result : Long_Integer := 0;
      Lsb_One : constant := 16#00000001#;
      Zero_Bits : constant := 16#00000000#;
   begin
      W :=  Interfaces.Shift_Left (Interfaces.Unsigned_32 (Bytes (Idx)), 24)
            or Interfaces.Shift_Left (Interfaces.Unsigned_32 (Bytes (Idx + 1)), 16)
            or Interfaces.Shift_Left (Interfaces.Unsigned_32 (Bytes (Idx + 2)), 8)
            or Interfaces.Unsigned_32 (Bytes (Idx + 3));
      for I in 0 .. 31 loop
         if (W and Interfaces.Shift_Left (Lsb_One, I)) /= Zero_Bits then
            if I = 31 then
               Result := Result + (-1 * (2 ** I));
            else
               Result := Result + (2 ** I);
            end if;
         end if;
      end loop;

      return Integer (Result);
   end To_Integer;

   procedure Handler (Socket : Sockets.Socket_Type) is
      Channel : Sockets.Stream_Access;
      Data : Streams.Stream_Element_Array (1 .. 9);
      Last : Streams.Stream_Element_Offset;
      Message_Type : Character;
      Asset_Prices : Prices.Set;
   begin
      Channel := Sockets.Stream (Socket);
      loop
         Streams.Read (Channel.all, Data, Last);
         exit when Last = 0;
         Message_Type := Character'Val (Data (1));
         --  for E of Data loop
         --   TIO.Put (Item => Hex (Byte => Interfaces.Unsigned_8 (E)) & " ");
         --  end loop;
         --  TIO.New_Line;

         if Message_Type = 'I' then
            declare
               Timestamp : constant Integer :=
                  To_Integer (Bytes => Data (2 .. 5));
               Price : constant Integer :=
                  To_Integer (Bytes => Data (6 .. 9));
            begin
               if Asset_Prices.Contains (Item => (Timestamp => Timestamp, Price => Price)) then
                  TIO.Put_Line ("already contains " & Timestamp'Image);
                  exit;
               else
                  Asset_Prices.Insert (New_Item => (Timestamp => Timestamp,
                                                    Price => Price));
               end if;
            end;
         elsif Message_Type = 'Q' then
            declare
               Min_Time : constant Integer :=
                  To_Integer (Bytes => Data (2 .. 5));
               Max_Time : constant Integer :=
                  To_Integer (Bytes => Data (6 .. 9));
               Mean_Value : constant Integer :=
                  Mean_Between (Asset_Prices => Asset_Prices,
                                Min_Time => Min_Time,
                                Max_Time => Max_Time);
            begin
               Write_Big_Endian (Stream => Channel,
                                 W => From_I (Mean_Value));
            end;
         else
            TIO.Put_Line ("Got undefined message type " & Message_Type);
            exit;
         end if;
      end loop;
      Sockets.Close_Socket (Socket);
   end Handler;

   procedure Run is
   begin
      Task_Per_Connection_Server.Run (Callback => Handler'Access);
   end Run;
end Means_To_An_End;
