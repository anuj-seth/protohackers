package Prime_Time.Json is
   Parse_Error : exception;
   procedure Parse (Json : String);
private
   type Lookahead_Predicate_Type is access function (X : Character)
      return Boolean;
end Prime_Time.Json;
