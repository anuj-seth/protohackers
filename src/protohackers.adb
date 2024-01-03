with Ada.Command_Line;
with Ada.Text_IO;

with Smoke_Test;
with Prime_Time;
with Means_To_An_End;

procedure Protohackers is
   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
begin
   if CLI.Argument_Count < 1 then
      TIO.Put_Line ("Please input test number to run");
      return;
   end if;
   if CLI.Argument (1) = "0" then
      Smoke_Test.Run;
   elsif CLI.Argument (1) = "1" then
      Prime_Time.Run;
   elsif CLI.Argument (1) = "2" then
      Means_To_An_End.Run;
   end if;
end Protohackers;
