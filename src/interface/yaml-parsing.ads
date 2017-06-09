with Yaml.Streams;
with Yaml.Sources;
private with Yaml.Lexing;
private with Yaml.Events;
private with Yaml.Strings;
private with Yaml.Stacks;
private with Yaml.String_Sets;

package Yaml.Parsing is
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
   package Tag_Handle_Sets is new Yaml.String_Sets (Strings.Content);

   type Parser_Implementation is new Streams.Stream_Implementation with record
      Pool : Strings.String_Pool;
      L : Lexing.Lexer;
      Levels : Level_Stacks.Stack;
      Current : Lexing.Token;
      Cached : Events.Event;
      Tag_Handles : Tag_Handle_Sets.String_Set;
      Header_Props, Inline_Props : Events.Properties;
      Header_Start, Inline_Start : Mark;
   end record;

   overriding procedure Close_Stream (Stream : in out Parser_Implementation);

   overriding procedure Fetch (Stream : in out Parser_Implementation;
                               E : out Events.Event);

   -----------------------------------------------------------------------------
   --  parser states
   -----------------------------------------------------------------------------

   --  starts the event stream by emitting a Stream_Start event
   function At_Stream_Start (P : in out Parser_Implementation'Class;
                             E : out Events.Event) return Boolean;

   --  final landing state. this just emits Stream_End events forever without
   --  reading anything.
   function At_Stream_End (P : in out Parser_Implementation'Class;
                           E : out Events.Event) return Boolean;

   --  state that expects a YAML document to start. reads directives.
   function Before_Doc (P : in out Parser_Implementation'Class;
                        E : out Events.Event) return Boolean;

   --  there may only be node properties and block scalar starts behind a '---'
   function After_Directives_End (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  state used when a document is started without explicit '---'. an explicit
   --  '---' is required for scalar documents.
   function Before_Implicit_Root (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  used when node properties occur at root levels. those *must* belong to
   --  a scalar which is an implicit map key and starts an implicit block map.
   function Require_Implicit_Map_Start (P : in out Parser_Implementation'Class;
                                        E : out Events.Event) return Boolean;

   --  used for parsing properties of a node.
   function Before_Node_Properties (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;

   --  state at the beginning of a block line. whether current implementation
   --  requires leaving block collections.
   function At_Block_Indentation (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  state inside a block line where a new node may start in compact notation,
   --  e.g. after a `-` or `:`. this only sets the node start position and then
   --  advances to After_Block_Parent_Props while possibly reading node
   --  properties in between.
   function After_Block_Parent (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;

   --  this either starts compact nodes or advances to the next line
   function After_Block_Parent_Props (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean;

   --  used when there are node properties at the beginning of a line. in that
   --  case, the corresponding node must start at the same line.
   function Require_Inline_Block_Item (P : in out Parser_Implementation'Class;
                                       E : out Events.Event) return Boolean;

   --  in this state, the document's root node has been closed and the only
   --  valid lexer tokens are '...', '---' or the end of the stream.
   function Before_Doc_End (P : in out Parser_Implementation'Class;
                            E : out Events.Event) return Boolean;
   function In_Block_Seq (P : in out Parser_Implementation'Class;
                          E : out Events.Event) return Boolean;
   function After_Implicit_Map_Start (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean;
   function Before_Block_Map_Key (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;
   function At_Block_Map_Key (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean;
   function At_Block_Map_Key_Props (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;
   function After_Implicit_Key (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;
   function Before_Block_Map_Value (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;
   function Before_Flow_Item (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean;
   function Before_Flow_Item_Props (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;
   function After_Flow_Map_Key (P : in out Parser_Implementation'Class;
                                 E : out Events.Event) return Boolean;
   function After_Flow_Map_Value (P : in out Parser_Implementation'Class;
                                   E : out Events.Event) return Boolean;
   function After_Flow_Seq_Item (P : in out Parser_Implementation'Class;
                                 E : out Events.Event) return Boolean;
   function After_Flow_Map_Sep (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;
   function After_Flow_Seq_Sep (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;
end Yaml.Parsing;
