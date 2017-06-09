with Yaml.Streams;
with Yaml.Sources;
private with Yaml.Lexing;
private with Yaml.Events;
private with Yaml.Strings;
private with Yaml.Stacks;

package Yaml.Parsing is
   pragma Preelaborate;

   Parser_Error : exception;

   type Parser is new Streams.Event_Stream with private;

   procedure Parse (P : in out Parser; Input : Sources.Source_Access);
   procedure Parse (P : in out Parser; Input : String);
private
   type Parser is new Streams.Event_Stream with null record;

   type Parser_Implementation;

   type Parser_State is access function
     (P : in out Parser_Implementation'Class; E : out Events.Event)
   return Boolean;

   subtype Indentation_Type is Integer range -2 .. Integer'Last;

   type Parsing_Level is record
      State : Parser_State;
      Indentation : Indentation_Type;
   end record;

   package Level_Stacks is new Yaml.Stacks (Parsing_Level);

   type Parser_Implementation is new Streams.Stream_Implementation with record
      Pool : Strings.String_Pool;
      L : Lexing.Lexer;
      Levels : Level_Stacks.Stack;
      Current : Lexing.Token;
      Cached : Events.Event;
      Implicit_Key : Boolean := False;
   end record;

   procedure Fetch (Stream : in out Parser_Implementation;
                    E : out Events.Event);

   -----------------------------------------------------------------------------
   --  parser states
   -----------------------------------------------------------------------------

   function At_Stream_Start (P : in out Parser_Implementation'Class;
                             E : out Events.Event) return Boolean;
   function At_Stream_End (P : in out Parser_Implementation'Class;
                           E : out Events.Event) return Boolean;
   function Before_Doc (P : in out Parser_Implementation'Class;
                         E : out Events.Event) return Boolean;
   function Before_Doc_End (P : in out Parser_Implementation'Class;
                            E : out Events.Event) return Boolean;
   function Before_Block_Item (P : in out Parser_Implementation'Class;
                               E : out Events.Event) return Boolean;
   function In_Block_Seq (P : in out Parser_Implementation'Class;
                          E : out Events.Event) return Boolean;
   function After_Implicit_Map_Start (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean;
   function Before_Block_Map_Key (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;
   function At_Block_Map_Key (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean;
   function After_Implicit_Key (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;
   function Before_Block_Map_Value (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;

end Yaml.Parsing;
