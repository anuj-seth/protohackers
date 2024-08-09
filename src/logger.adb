with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Task_Identification;

package body Logger is
   procedure Log (Item : String) is
      Now : constant String :=
         Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
      Task_Id : constant String :=
         Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task);
      Message : constant String :=
         Now & " " & Task_Id & " " & Item;
   begin
      Ada.Text_IO.Put_Line (Message);
   end Log;
end Logger;
