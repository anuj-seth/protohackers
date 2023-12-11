with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

package body Prime_Time.Json is
   package TIO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;
   package CL1 renames Ada.Characters.Latin_1;

   function Is_Space (X : Character) return Boolean is
      (CH.Is_Space (Item => X) or else X = CL1.HT);

   function Is_Alpha (X : Character) return Boolean
      renames CH.Is_Letter;

   function Is_Digit (X : Character) return Boolean
      renames CH.Is_Digit;

   function Is_Alphanumeric (X : Character) return Boolean is
      (Is_Alpha (X => X) or else Is_Digit (X => X));

   function Is_Not_Quote (X : Character) return Boolean is
      (X /= '"');

   function Increment_Look_Index (Index : Positive;
      Last : Positive) 
   return Positive is
   begin
      if Index < Last then 
         return Index + 1;
      else
         return Index;
      end if;
   end Increment_Look_Index;

   procedure Skip_Whitespace (S : String; 
      Idx : in out Positive) is
   begin
      while Is_Space (X => S (Idx)) loop
         Idx := Increment_Look_Index (Index => Idx,
                                      Last => S'Length);
      end loop;
   end Skip_Whitespace;

   procedure Match (X : Character;
      S : String;
      Idx : in out Positive) is
   begin
      if S (Idx) = X then
        Idx := Increment_Look_Index (Index => Idx,
                                     Last => S'Last);
      else
         raise Parse_Error with "Expected: " & X & " Found:" & S (Idx);
      end if;
   end Match;

   function Get_Char_While_True (S : String;
      Idx : in out Positive;
      Predicate : Lookahead_Predicate_Type;
      Till_Now : String) 
   return String is
   begin
      if Predicate (X => S (Idx)) then
         declare
            Name : String (1 .. Till_Now'Length + 1);
         begin
            Name (1 .. Name'Length - 1) := Till_Now;
            Name (Name'Length) := S (Idx);
            Idx := Increment_Look_Index (Index => Idx,
                                         Last => S'Last); 
            return Get_Char_While_True (S => S,
                                        Idx => Idx,
                                        Predicate => Predicate,
                                        Till_Now => Name);
         end;
      else
         return Till_Now;
      end if;
   end Get_Char_While_True;

   function Get_String (S : String; Idx : in out Positive)
   return String is
   begin
      Match (X => '"', S => S, Idx => Idx);
      declare
         Result : constant String :=
            Get_Char_While_True (S => S,
                                 Idx => Idx,
                                 Predicate => Is_Not_Quote'Access,
                                 Till_Now => "");
      begin
         Match (X => '"', S => S, Idx => Idx);
         return Result;
      end;
   end Get_String;

   function Get_Number (S : String; Idx : in out Positive)
   return String is
   begin
      Skip_Whitespace (S => S, Idx => Idx);
      return Get_Char_While_True (S => S,
                                  Idx => Idx,
                                  Predicate => Is_Digit'Access,
                                  Till_Now => "");
   end Get_Number;

   function Key (S : String; Idx : in out Positive)
   return String is
   begin
      Skip_Whitespace (S => S, Idx => Idx);
      declare
         Result : constant String :=
            Get_String (S => S,
                        Idx => Idx);
      begin
         Skip_Whitespace (S => S, Idx => Idx);
         Match (X => ':', S => S, Idx => Idx);
         return Result;
      end;
   end Key;

   function Value (S : String; Idx : in out Positive)
   return String is
   begin
      Skip_Whitespace (S => S, Idx => Idx);
      if S (Idx) = '"' then
         return Get_String (S => S,
                            Idx => Idx);
      else
         return Get_Number (S => S,
                            Idx => Idx);
      end if;
   end Value;

   procedure Parse (Json : String) is
      Look_Index : Positive := Json'First;
   begin
      TIO.Put_Line (Item => Json);
      TIO.Put_Line (Item => Json'First'Image);
      Skip_Whitespace (S => Json, Idx => Look_Index);
      Match (X => '{', S => Json, Idx => Look_Index);
      while True loop
         declare
            K : constant String :=
               Key (S => Json, Idx => Look_Index);
            V : constant String :=
               Value (S => Json, Idx => Look_Index);

         begin
            TIO.Put_Line (K & ":" & V);
            Skip_Whitespace (S => Json, Idx => Look_Index);
            exit when Look_Index = Json'Length;
            Match (X => ',', S => Json, Idx => Look_Index);
         end;
      end loop;
      Skip_Whitespace (S => Json, Idx => Look_Index);
      Match (X => '}', S => Json, Idx => Look_Index);
      TIO.Put_Line (Item => Look_Index'Image);
   end Parse;
end Prime_Time.Json;
