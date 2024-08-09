with Ada.Command_Line;
with Ada.Text_IO;

with Smoke_Test;
with Prime_Time;
with Means_To_An_End;
with Budget_Chat;
with Unusual_Database;
with Mob_In_The_Middle;
with Speed_Daemon.Server;

procedure Protohackers is
   type Program_Id is range 0 .. 12;
   Program : Program_Id;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Please input test number to run");
      return;
   end if;
   Program := Program_Id'Value (Ada.Command_Line.Argument (1));
   case Program is
      when 0 =>
         Smoke_Test.Run;
      when 1 =>
         Prime_Time.Run;
      when 2 =>
         Means_To_An_End.Run;
      when 3 =>
         Budget_Chat.Run;
      when 4 =>
         Unusual_Database.Run;
      when 5 =>
         Mob_In_The_Middle.Run;
      when 6 =>
         Speed_Daemon.Server.Run;
      when others =>
         Ada.Text_IO.Put_Line (Item => "Not implemented");
   end case;
end Protohackers;
