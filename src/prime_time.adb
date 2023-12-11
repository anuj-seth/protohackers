with Ada.Text_IO;

with Prime_Time.Json;

package body Prime_Time is
   procedure Run is
   begin
      Prime_Time.Json.Parse (Json => "{""method"": ""isPrime"", ""number"":123 }");
   end Run;
end Prime_Time;
