with Yada.Sources;
private with Ada.Strings.Maps;

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

   --  The following stuff is declared here so that it can be unit-tested.

   function Next (Object : in out Lexer) return Character with Inline;
   procedure Handle_CR (L : in out Lexer);
   procedure Handle_LF (L : in out Lexer);

   End_Of_Input    : constant Character := Character'Val (4);
   Line_Feed       : constant Character := Character'Val (10);
   Carriage_Return : constant Character := Character'Val (13);

   Line_Ends : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Line_Feed & Carriage_Return);
end Yada.Lexing;
