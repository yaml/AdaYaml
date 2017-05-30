with Yada.Sources;
private with Ada.Strings.Maps;

private package Yada.Lexing is
   Default_Initial_Buffer_Size : constant := 8096;

   type Lexer is limited private;

   Lexer_Error : exception;

   function From_Source
     (Input : Sources.Source_Access;
      Initial_Buffer_Size : Positive := Default_Initial_Buffer_Size)
      return Lexer;

   function From_String (Input : String) return Lexer;
   function From_String (Input : not null access String) return Lexer;

   type Token is
     (Yaml_Directive,    --  `%YAML`
      Tag_Directive,     --  `%TAG`
      Unknown_Directive, --  any directive but `%YAML` and `%TAG`
      Directive_Param,   --  parameters of `%YAML` or `%TAG`
      Empty_Line,        --  necessary for correctly handling line folding in
                         --  multiline plain scalars
      Directives_End,    --  explicit `---`
      Document_End,      --  explicit `...`
      Stream_End,        --  end of input
      Indentation,       --  yielded at beginning of non-empty line
      Scalar_Indicator,  --  used to ask the parser to tell the lexer the
                         --  current parent's indentation. This is necessary for
                         --  the lexer to know when to end scanning for the
                         --  scalar' content. Used for plain and block scalars.
      Scalar_Content,    --  content of a scalar.
      Seq_Item_Ind,      --  block sequence item indicator `- `
      Map_Key_Ind,       --  mapping key indicator `? `
      Map_Value_Ind,     --  mapping value indicator `: `
      Flow_Map_Start,    --  `{`
      Flow_Map_End,      --  `}`
      Flow_Seq_Start,    --  `[`
      Flow_Seq_End,      --  `]`
      Flow_Separator,    --  `,`
      Tag_Handle,        --  a handle of a tag shorthand, e.g. `!!` of `!!str`
      Tag_Suffix,        --  suffix of a tag shorthand, e.g. `str` of `!!str`
      Literal_Tag,       --  a literal tag, e.g. `!<tag:yaml.org,2002:str>`
      Anchor,            --  an anchor property of a node, e.g. `&anchor`
      Alias,             --  an alias property of a node, e.g. `*alias`
      Attribute          --  an attribute property of a node, e.g. `@attribute`
     );

   function Next_Token (L : in out Lexer) return Token with Inline;

   --  tell the lexer the indentation of the current parent element. this proc
   --  shall be called whenever the Lexer emits a Scalar_Indicator.
   procedure Tell_Indentation (L : in out Lexer; Indentation : Natural)
     with Inline;

   --  return the lexeme of the recent token without the first character. This
   --  is useful for anchors, aliases, directive names and the like.
   function Short_Lexeme (L : Lexer) return String with Inline;

   --  return the current lexeme including the first character. This is useful
   --  for tokens that do not have a leading indicator char.
   function Full_Lexeme (L : Lexer) return String with Inline;
private
   type Buffer_Type is access all String;

   type Lexer_State is not null access function
     (L : in out Lexer; T : out Token) return Boolean;

   type Lexer is limited record
      Cur_Line    : Positive;
      Token_Start : Positive;
      Line_Start  : Positive;
      Input       : Sources.Source_Access;
      Sentinel    : Positive;
      Pos         : Positive;
      Buffer      : Buffer_Type;
      Indentation : Natural;
      State       : Lexer_State;
      Cur         : Character;
   end record;

   --  The following stuff is declared here so that it can be unit-tested.

   --  buffer handling

   function Next (Object : in out Lexer) return Character with Inline;
   procedure Handle_CR (L : in out Lexer);
   procedure Handle_LF (L : in out Lexer);

   End_Of_Input    : constant Character := Character'Val (4);
   Line_Feed       : constant Character := Character'Val (10);
   Carriage_Return : constant Character := Character'Val (13);

   Line_Ends : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Line_Feed & Carriage_Return);
   Space_Or_Line_End : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (' ' & Line_Feed & Carriage_Return & End_Of_Input);
   Comment_Or_Line_End : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ('#' & Line_Feed & Carriage_Return & End_Of_Input);

   --  tokenization
   function Outside_Doc (L : in out Lexer; T : out Token) return Boolean;
   function Yaml_Version (L : in out Lexer; T : out Token) return Boolean;
   function Tag_Shorthand (L : in out Lexer; T : out Token) return Boolean;
   function Tag_Uri (L : in out Lexer; T : out Token) return Boolean;
   function Unknown_Directive (L : in out Lexer; T : out Token) return Boolean;
   function Stream_End (L : in out Lexer; T : out Token) return Boolean;
   function Inside_Line (L : in out Lexer; T : out Token) return Boolean;
end Yada.Lexing;
