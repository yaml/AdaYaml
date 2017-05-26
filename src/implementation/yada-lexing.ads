with Yada.Sources;

private package Yada.Lexing is
   Default_Initial_Buffer_Size : constant := 8096;

   type Lexer is limited private;

   function From_Source
     (Input : Sources.Source_Access;
      Initial_Buffer_Size : Positive := Default_Initial_Buffer_Size)
      return Lexer;

private
   type Buffer_Type is access String;

   type Lexer is limited record
      Input    : Sources.Source_Access;
      Sentinel : Positive;
      Pos      : Positive;
      Buffer   : Buffer_Type;
   end record;
end Yada.Lexing;
