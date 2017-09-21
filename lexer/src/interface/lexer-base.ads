with Ada.Finalization;

with Lexer.Source;

package Lexer.Base is
   pragma Preelaborate;

   Default_Initial_Buffer_Size : constant := 8192;

   type Buffer_Type is access all String;

   type Private_Values is limited private;

   type Instance is limited new Ada.Finalization.Limited_Controlled with record
      Cur_Line    : Positive := 1;
        --  index of the line at the current position
      Line_Start  : Positive := 1;
        --  the buffer index where the current line started
      Prev_Lines_Chars : Natural := 0;
        --  number of characters in all previous lines,
        --  used for calculating index.
      Pos         : Positive := 1;
        --  position of the next character to be read from the buffer
      Buffer      : Buffer_Type;  --  input buffer. filled from the source.
      Internal : Private_Values;
   end record;

   procedure Init (Object : in out Instance; Input : Source.Pointer;
                   Initial_Buffer_Size : Positive :=
                     Default_Initial_Buffer_Size);
   procedure Init (Object : in out Instance; Input : String);

   subtype Rune is Wide_Wide_Character;

   function Next (Object : in out Instance) return Character with Inline;
   --  function Next (Object : in out Instance) return Rune with Inline;

   procedure Handle_CR (L : in out Instance);
   procedure Handle_LF (L : in out Instance);
private
   type Private_Values is limited record
      Input       : Source.Pointer;  --  input provider
      Sentinel    : Positive;
      --  the position at which, when reached, the buffer must be refilled
   end record;

   overriding procedure Finalize (Object : in out Instance);

   procedure Refill_Buffer (L : in out Instance);
end Lexer.Base;
