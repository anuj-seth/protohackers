with GNAT.Sockets;
with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Speed_Daemon is
   type Device_Type is (Camera, Dispatcher, Unknown);
   type Road_Number is new Natural range 0 .. 65535;
   type Response_Types is (Error, Ticket, Heartbeat);
   --  Response_To_Network_Type : array (Response_Types) of Interfaces.Unsigned_32 :=
   --   (Error => 16#10#,
   --   Ticket => 16#21#,
   --   Heartbeat => 16#41#);

   type Request_Types is (Plate, Want_Heartbeat, I_Am_Camera,
      I_Am_Dispatcher, Unknown_Message);
   --  Network_To_Request_Type := array (Interfaces.Unsigned_32) of Request_Types :=
   --   (16#20# => Plate,
   --   16#40# => Want_Heartbeat,
   --   16#80# => I_Am_Camera,
   --   16#81# => I_Am_Dispatcher);

   type Request (Tp : Request_Types) is
      record
         case Tp is
            when Want_Heartbeat =>
               Interval : Natural;
            when I_Am_Camera =>
               Road : Road_Number;
               Mile : Positive;
               Speed_Limit : Positive;
            when others =>
               null;
         end case;
      end record;

   package Roads is 
      new Ada.Containers.Vectors (Index_Type => Positive,
         Element_Type => Road_Number);

   type Device (Tp : Device_Type) is
      record
         case Tp is
            when Camera => 
               Road : Road_Number;
               Mile : Positive;
               Speed_Limit : Positive;
            when Dispatcher =>
               For_Roads : Roads.Vector;
            when Unknown =>
               null;
         end case;
      end record;

   function Socket_Hash (Key : GNAT.Sockets.Socket_Type)
      return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (GNAT.Sockets.Image (Socket => Key)));
   use type GNAT.Sockets.Socket_Type;
end Speed_Daemon;
