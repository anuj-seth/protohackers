with Ada.Text_IO;
with GNATCOLL.JSON;   use type GNATCOLL.JSON.JSON_Value_Type;
with Ada.Numerics.Elementary_Functions;

package body Prime_Time.Message is
   package TIO renames Ada.Text_IO;
   package Json renames GNATCOLL.JSON;

   function Is_Prime (N : Integer)
   return Boolean is
      use Ada.Numerics.Elementary_Functions;
   begin
      if N <= 0 then
         return False;
      elsif N = 2 then
         return True;
      elsif N mod 2 = 0 then
         return False;
      else
         for I in 3 .. Positive (Sqrt (Float (N))) loop
            if N mod I = 0 then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Is_Prime;

   function Is_Fractional_Part_Zero (Number : Float)
   return Boolean is
      (Float'Rounding (Number) = Number);

   function Is_Prime (N : Float)
   return Boolean is
   begin
      if Is_Fractional_Part_Zero (Number => N) then
         return Is_Prime (N => Integer (N));
      else
         return False;
      end if;
   end Is_Prime;

   function Are_Required_Fields_Present (Json_Data : Json.JSON_Value)
   return Boolean is
      (Json.Has_Field (Val => Json_Data,
                       Field => "method")
       and then
       Json.Has_Field (Val => Json_Data,
                       Field => "number"));

   function Are_Fields_Of_Correct_Type (Json_Data : Json.JSON_Value)
   return Boolean is
      (Json.Get (Val => Json_Data,
                 Field => "method").Kind = Json.JSON_String_Type
       and then
       (Json.Get (Val => Json_Data,
                  Field => "number").Kind = Json.JSON_Float_Type
        or else
        Json.Get (Val => Json_Data,
                  Field => "number").Kind = Json.JSON_Int_Type));

   function Is_Method_isPrime (Json_Data : Json.JSON_Value)
   return Boolean is
      (Json.Get (Val => Json_Data, Field => "method") = "isPrime");

   function Parse (Json_String : String;
      Parsed_Json : out Json.JSON_Value)
   return Boolean is
      Result : constant Json.Read_Result :=
         Json.Read (Json_String);
   begin
      TIO.Put_Line (Item => Json_String);
      if Result.Success
         and then Json.Kind (Val => Result.Value) = Json.JSON_Object_Type
      then
         Parsed_Json := Result.Value;
         return True;
      else
         return False;
      end if;
   end Parse;

   function Message_Handler (Json_String : String;
      Success : out Boolean)
   return String is
      Parsed_Json : Json.JSON_Value;
      Is_Prime_Number : Boolean;
      Float_Number : Float;
      Integer_Number : Long_Integer;
      Response : constant Json.JSON_Value := Json.Create_Object;
   begin
      if Parse (Json_String => Json_String,
                Parsed_Json => Parsed_Json)
         and then Are_Required_Fields_Present (Json_Data => Parsed_Json)
         and then Are_Fields_Of_Correct_Type (Json_Data => Parsed_Json)
         and then Is_Method_isPrime (Json_Data => Parsed_Json)
      then
         if Json.Get (Val => Parsed_Json,
                      Field => "number").Kind = Json.JSON_Float_Type
         then
            Float_Number := Json.Get (Val => Parsed_Json,
                                      Field => "number");
            Is_Prime_Number := Is_Prime (N => Float_Number);
         else
            Integer_Number := Json.Get (Val => Parsed_Json,
                                        Field => "number");
            Is_Prime_Number := Is_Prime (N => Integer (Integer_Number));
         end if;
         Response.Set_Field ("method", "isPrime");
         Response.Set_Field ("prime", Is_Prime_Number);
         Success := True;
      else
         Response.Set_Field ("status", "error");
         Success := False;
      end if;
      return Response.Write;
   end Message_Handler;
end Prime_Time.Message;
