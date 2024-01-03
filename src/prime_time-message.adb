pragma Ada_2022;
with Ada.Text_IO;
with GNATCOLL.JSON;   use type GNATCOLL.JSON.JSON_Value_Type;
with GNAT.Regpat;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Big_Numbers.Big_Integers;

package body Prime_Time.Message is
   package TIO renames Ada.Text_IO;
   package Json renames GNATCOLL.JSON;
   package BINT renames Ada.Numerics.Big_Numbers.Big_Integers;
   use type BINT.Big_Integer;
   package Regpat renames GNAT.Regpat;

   function Get_Big_Number (S : String)
   return BINT.Big_Integer is
      Re : constant Regpat.Pattern_Matcher :=
         Regpat.Compile (".*""number""\s*:\s*(\d*),");
      Matches : Regpat.Match_Array (0 .. 1);
   begin
      Regpat.Match (Re, S, Matches);
      return BINT.From_String (Arg => S (Matches (1).First .. Matches (1).Last));
   end Get_Big_Number;

   function Is_Prime (N : Long_Integer)
   return Boolean is
      use Ada.Numerics.Elementary_Functions;
   begin
      if N <= 1 then
         return False;
      elsif N = 2 then
         return True;
      elsif N mod 2 = 0 then
         return False;
      else
         for I in 3 .. Positive (Sqrt (Float (N))) loop
            if N mod Long_Integer (I) = 0 then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Is_Prime;

   function Is_Prime (N : BINT.Big_Integer)
   return Boolean is
      I : BINT.Big_Integer := BINT.To_Big_Integer (Arg => 3);
   begin
      if N <= 1 then
         return False;
      elsif N = 2 then
         return True;
      elsif N mod 2 = 0 then
         return False;
      else
         while I < N - 1 loop
            if N mod I = 0 then
               return False;
            end if;
            I := I + 1;
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
         return Is_Prime (N => Long_Integer (N));
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

   function Is_Big_Number (Json_Data : Json.JSON_Value)
   return Boolean is
      (Json.Has_Field (Val => Json_Data,
                       Field => "bignumber")
       and then Json.Get (Val => Json_Data,
                          Field => "bignumber") = True);

   function Parse (Json_String : String;
      Parsed_Json : out Json.JSON_Value)
   return Boolean is
      Result : constant Json.Read_Result :=
         Json.Read (Json_String);
   begin
      if Result.Success
         and then Json.Kind (Val => Result.Value) = Json.JSON_Object_Type
      then
         Parsed_Json := Result.Value;
         return True;
      else
         TIO.Put_Line (Item => Json.Format_Parsing_Error (Error => Result.Error));
         return False;
      end if;
   end Parse;

   function Message_Handler (Json_String : String;
      Success : out Boolean)
   return String is
      Parsed_Json : Json.JSON_Value;
      Is_Prime_Number : Boolean := False;
      Float_Number : Float;
      Number_Value : Json.JSON_Value;
      Integer_Number : Long_Integer;
      Response : constant Json.JSON_Value := Json.Create_Object;
   begin
      if not (Parse (Json_String => Json_String,
                     Parsed_Json => Parsed_Json)
              and then Are_Required_Fields_Present (Json_Data => Parsed_Json)
              and then Are_Fields_Of_Correct_Type (Json_Data => Parsed_Json)
              and then Is_Method_isPrime (Json_Data => Parsed_Json))
      then
         Response.Set_Field ("status", "error");
         Success := False;
         return Response.Write;
      end if;

      if Is_Big_Number (Json_Data => Parsed_Json) then
         Is_Prime_Number := Is_Prime (N => Get_Big_Number (S => Json_String));
      else
         Number_Value := Json.Get (Val => Parsed_Json,
                                   Field => "number");
         if Number_Value.Kind = Json.JSON_Float_Type then
            Float_Number := Json.Get (Val => Number_Value);
            Is_Prime_Number := Is_Prime (N => Float_Number);
         else
            Integer_Number := Json.Get (Val => Number_Value);
            Is_Prime_Number := Is_Prime (N => Integer_Number);
         end if;
      end if;
      Response.Set_Field ("method", "isPrime");
      Response.Set_Field ("prime", Is_Prime_Number);
      Success := True;
      return Response.Write;
   end Message_Handler;
end Prime_Time.Message;
